(ns #^{:author "Nurullah Akkaya"}
  alter-ego.core
  (:refer-clojure :exclude [sequence])
  (:use [clojure.java.shell :only [sh]]
        [clojure.pprint :only [write code-dispatch]]
        [clojure.stacktrace :only [root-cause]]))

(def alter-ego-pool-freq (/ 1000 30))

(defmulti exec 
  "Given a node dispatch to its exec implementation."
  (fn [node & [terminate?]]
    (node :type)))

(defn- exec-action? [terminate?]
  (cond (nil? terminate?) true
        (not @terminate?) true
        :default false))

(defn- terminate [a]
  (swap! a (fn [_] (identity true))))

(defn- default-doc [type]
  (let [capitalize #(str (Character/toUpperCase (.charAt % 0))
                         (.toLowerCase (subs % 1)))
        doc (map capitalize
                 (-> (name type)
                     (.split "-")))]
    (apply str (interpose \space doc))))

(defn- parse-children [children type]
  (let [doc (if (string? (first children))
              (str (first children) "\\n" (name type))
              (default-doc type))
        children (if (string? (first children))
                   (next children)
                   children)
        id (if (symbol? (first children))
               (first children)
               (gensym "N_"))
        children (if (symbol? (first children))
                   (next children)
                   children)]

    [doc id children]))

;;
;; Leafs
;;

(def alter-ego-trace-logger (atom nil))

(defn alter-ego-set-logger [f]
  (reset! alter-ego-trace-logger f))

(defn alter-ego-unmangle
  [class-name]
  (.replace
   (clojure.string/replace class-name #"^(.+)\$([^@]+)(|@.+)$" "$1/$2")
   \_ \-))

(defmacro current-fn-name []
  "Returns a string, the name of the current Clojure function"
  `(-> (Throwable.) .getStackTrace first .getClassName alter-ego-unmangle))

(defmacro trace
  [c]
  `{:trace (current-fn-name)
    :type :trace :children ~c :doc "trace" :id (gensym "N_")})

(defmethod exec :trace [{children :children trace :trace} & [terminate?]]
  (try
    (exec children terminate?)
    (catch Exception e
      (when @alter-ego-trace-logger
        (@alter-ego-trace-logger
         (str "Trace=> " trace)))
      (throw e))))

(defmacro action [& body]
  (let [[doc _ body] (parse-children body :action)]
    {:type :action :trace `(quote (action ~@body)) :doc doc :id '(gensym "N_") :action `(fn [] ~@body)}))

(def ^{:private true} unit->ms
  {:microsecond 0.001 :millisecond 1
   :second 1000 :minute 60000
   :hour 3600000 :day 86400000
   :month 2678400000})

(defn throttler [f rate unit]
  (let [rate-ms (/ (unit->ms unit) rate)
        ;;rate-ms (/ rate (unit->ms unit))
        t-last-run (atom (System/currentTimeMillis))
        throttled-fn (fn [f]
                       (fn [& args]
                         (let [elapsed (- (System/currentTimeMillis) @t-last-run)
                               left-to-wait (- rate-ms elapsed)]
                           (when (pos? left-to-wait)
                             (Thread/sleep left-to-wait)))
                         (let [ret (apply f args)]
                           (reset! t-last-run (System/currentTimeMillis))
                           ret)))]
    (throttled-fn f)))

(defmacro throttled-action [rate unit & body]
  (let [[doc _ body] (parse-children body :action)]
    {:type :action :trace `(quote (throttled-action ~@body))
     :doc doc :id '(gensym "N_") :action `(throttler (fn [] ~@body) ~rate ~unit)}))

(defmethod exec :action
  [{action :action trace :trace} & [terminate?]]
  (try
    (if (exec-action? terminate?)
      (boolean (action))
      false)
    (catch Exception e
      (when @alter-ego-trace-logger
        (@alter-ego-trace-logger
         (str (root-cause e) "\n"
              (write trace :dispatch code-dispatch :stream nil))))
      (throw e))))

;;
;; Selectors
;;

(defn selector 
  "Tries to run all its children in sequence as soon as one succeeds 
   it also succeeds."
  [& children]
  (let [[doc id children] (parse-children children :selector)]
    {:type :selector :children children :doc doc :id id}))

(defn non-deterministic-selector 
  "Same as selector, but shuffles all its children prior to execution."
  [& children]
  (let [[doc id children] (parse-children children :non-deterministic-selector)]
    {:type :non-deterministic-selector :children children :doc doc :id id}))

(defn- select [children terminate?]
  (if (exec-action? terminate?)
    (if-let[s (seq children)] 
      (if-not (exec (first s) terminate?)
        (recur (rest s) terminate?)
        true)
      false)
    false))

(defmethod exec :selector [{children :children} & [terminate?]]
  (if (not (true? (select children terminate?))) false true))

(defmethod exec :non-deterministic-selector [{children :children} & [terminate?]]
  (if (not (true? (select (shuffle children) terminate?))) false true))

;;
;; Sequences
;;

(defn sequence 
  "Runs all of its children in sequential order. If one of them fails, 
   it also fails. Once all of them succeeds, it also succeeds."
  [& children]
  (let [[doc id children] (parse-children children :sequence)]
    {:type :sequence :children children :doc doc :id id}))

(defn non-deterministic-sequence 
  "Same as sequence, but shuffles all its children prior to execution."
  [& children]
  (let [[doc id children] (parse-children children :non-deterministic-sequence)]
    {:type :non-deterministic-sequence :children children :doc doc :id id}))

(defn- seq-run [children terminate?]
  (if (exec-action? terminate?)
    (if-let [s (seq children)]
      (if (exec (first s) terminate?)
        (recur (rest s) terminate?)
        false)
      true)
    false))

(defmethod exec :sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run children terminate?))) true false))

(defmethod exec :non-deterministic-sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run (shuffle children) terminate?))) true false))

(defn parallel
  "Concurrently executes all its children. If policy is :sequence, it acts as a sequence.
   If the policy is :selector it acts as a selector."
  [& xs]
  (let [[doc id [policy & children]] (parse-children xs :parallel)]
    {:type :parallel :policy policy :children children :doc (str doc " - " (name policy)) :id id}))

(let [all-succeded? #(and (every? true? (map future-done? %))
                          (every? true? (map deref %)))
      
      all-failed? #(and (every? true? (map future-done? %))
                        (every? false? (map deref %)))

      some-succeded? #(some true? (map deref (filter future-done? %)))
      
      some-failed? #(some false? (map deref (filter future-done? %)))
      
      cancel (fn [fs atom return]
               (terminate atom)
               (doall (map deref fs)) return)]

  (defmethod exec :parallel [{children :children policy :policy} & [terminate?]]
    (if (exec-action? terminate?)
      (let [parent-terminate? terminate?
            self-terminate? (atom false)
            fs (map #(future (exec % self-terminate?)) children)]
        (if (= policy :sequence)
          (try
            (loop []
              (Thread/sleep alter-ego-pool-freq)
              (cond (all-succeded? fs) true
                    (some-failed? fs)  (cancel fs self-terminate? false)
                    (not (exec-action? parent-terminate?)) (cancel fs self-terminate? false)
                    :default (recur)))
            (catch Exception e
              (cancel fs self-terminate? false)
              (throw e)))

          (try
            (loop []
              (Thread/sleep alter-ego-pool-freq)
              (cond (all-failed? fs) false
                    (some-succeded? fs) (cancel fs self-terminate? true)
                    (not (exec-action? parent-terminate?)) (cancel fs self-terminate? false)
                    :default (recur)))
            (catch Exception e
              (cancel fs self-terminate? false)
              (throw e)))))
      false)))

;;
;; Decorators
;;

(defn forever
  "When its child task finishes, it runs it once more."
  [c]
  {:type :forever :children c :doc (default-doc :forever) :id (gensym "N_")})

(defmethod exec :forever [{children :children} & [terminate?]]
  (loop []
    (if (exec-action? terminate?)
      (do (exec children terminate?)
          (recur))
      false)))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  {:type :until-fail :children c :doc (default-doc :until-fail) :id (gensym "N_")})

(defmethod exec :until-fail [{children :children} & [terminate?]]
  (loop []
    (if (and (exec-action? terminate?)
             (exec children terminate?))
      (recur)
      true)))

(defn until-success
  "Runs its children until it returns true."
  [c]
  {:type :until-success :children c :doc (default-doc :until-success) :id (gensym "N_")})

(defmethod exec :until-success [{children :children} & [terminate?]]
  (loop []
    (if (and (exec-action? terminate?)
             (not (exec children terminate?)))
      (recur)
      true)))

(defn limit 
  "Unless its children succeeds will keep running it at most i times."
  [i children]
  {:type :limit :children children :times i :doc (str (default-doc :limit) " - " i) :id (gensym "N_")})

(defmethod exec :limit [{children :children times :times} & [terminate?]]
  (loop [i times]
    (if (and (pos? i)
             (exec-action? terminate?))
      (if (not (exec children terminate?)) 
        (recur (dec i))
        true)
      false)))

(defn inverter 
  "Inverts its childrens return value, succees becames failure and 
   vice versa."
  [c]
  {:type :inverter :children c :doc (default-doc :inverter) :id (gensym "N_")})

(defmethod exec :inverter [{children :children} & [terminate?]]
  (not (exec children terminate?)))

(defn interrupter
  "Lets its child node run normally. If the child returns a result,
   it passes that result on up the tree. But, if the child is still working,
   and watcher returns a result it will terminate the child and return the
   result of perform. [watch child perform]"
  [& children]
  {:type :interrupter :children children :doc (default-doc :interrupter) :id (gensym "N_")})

(defmethod exec :interrupter
  [{[watch children perform] :children} & [terminate?]]
  (if (exec-action? terminate?)
    (let [parent-terminate? terminate?
          terminate-children? (atom false)
          terminate-watch? (atom false)
          children (future (exec children terminate-children?))
          watch (future (exec watch terminate-watch?))]

      (loop []
        (Thread/sleep alter-ego-pool-freq)
        (cond (future-done? children) (do (terminate terminate-watch?)
                                          (try @children (catch Exception e (throw e)))
                                          (deref watch)
                                          (deref children))

              (future-done? watch) (do (terminate terminate-children?)
                                       (try @watch (catch Exception e (throw e)))
                                       (deref children)
                                       (exec perform))

              (not (exec-action? parent-terminate?)) (do (terminate terminate-children?)
                                                         (terminate terminate-watch?)
                                                         (deref watch)
                                                         (deref children)
                                                         (exec perform)
                                                         false)
              
              :default (recur))))
    false))

(defn exec-repl
  "Exec given behaviour, when the user presses a key interrupt it and run cleanup action
   unless it completes."
  [behaviour cleanup-when-interrupted]
  (let [read-line (resolve 'read-line)
        stop? (atom false)
        done? (atom false)]
    (future (try
              (exec (interrupter
                     (until-success
                      (action (Thread/sleep alter-ego-pool-freq)
                              (deref stop?)))
                     (selector (sequence behaviour
                                         (action (swap! done? not)))
                               (action (swap! done? not)))
                     (action true)))
              (catch Exception e
                (flush)
                (swap! done? not)
                (exec cleanup-when-interrupted))))
    
    (println "Press Enter to Interrupt")
    
    (while (and (not (read-line))
                (not @done?))
      (Thread/sleep alter-ego-pool-freq))
    
    (swap! stop? not)
    
    (if (and (not @done?)
             @stop?)
      (exec cleanup-when-interrupted))))

(defmacro dynamic
  "Allows building of trees at run time, dynamic
   node executes its body and runs the returned tree."
  [& body]
  `{:type :dynamic :children (fn [] ~@body) :doc "Dynamic" :id (gensym "N_")})

(defmethod exec :dynamic [{children :children} & [terminate?]]
  (exec (children) terminate?))

;;
;; Graphviz 
;;

(defn- graph
  ([tree skip]
     (let [subgraphs (atom #{})
           buffer (StringBuffer.)
           add (fn [& xs]
                 (.append buffer (apply str xs)))]
       (add "digraph bt {\n"
            "graph [ordering=\"out\",rankdir=\"TB\"];"
            "node [fontname=Arial, style=\"rounded\", shape=box]"
            "edge [color=\"0.650 0.700 0.700\"]")
       (graph add skip subgraphs tree nil)
       (add "}")
       (.toString buffer)))
  ([add skip subgraphs node parent-id]
     (if (skip (:id node))
       (let [node-id (gensym "N_")]
         (add node-id " [label=\"" (:id node) "\",style=\"filled\",fillcolor=\"#00ff005f\"];\n")
         (add parent-id " -> "  node-id "\n"))
       (if ((:id node) @subgraphs)
         (add parent-id " -> "  ((:id node) @subgraphs) "\n")
         (let [{:keys [doc id children type]} node
               children (cond (seq? children) children
                              (nil? children) []
                              :default [children])
               style (if (= type :action)
                       ",style=\"filled\",fillcolor=\"#CCFFFF\"" "")]

           (add id " [label=\"" doc "\"" style "];\n")
           
           (when parent-id
             (add parent-id " -> "  id "\n"))

           (swap! subgraphs conj id)

           (doseq [c children]
             (graph add skip subgraphs c id)))))))

(defn- render [dot]
  (:out (sh "dot" "-Tpng" :in dot :in-enc "UTF-8" :out-enc :bytes)))

(defn export [tree f fmt & [skip]]
  (let [dot (graph tree (if skip
                          skip #{}))]
    (condp = fmt
      :dot (spit f dot)
      :png (let [bytes (render dot)]
             (with-open [f (java.io.FileOutputStream. f)]
               (.write f bytes)))
      :default (throw (Exception. "Unknown Format")))))

(defn show [tree & [skip]]
  (let [dot (graph tree (if skip
                          skip #{}))
        bytes (render dot)
        image (javax.swing.ImageIcon. bytes)]

    (doto (javax.swing.JFrame.)
      (.add (javax.swing.JLabel. image))
      (.pack)
      (.show))))
