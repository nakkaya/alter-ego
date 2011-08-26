(ns #^{:author "Nurullah Akkaya"}
  alter-ego.core
  (:refer-clojure :exclude [sequence]))

(defmulti exec 
  "Given a node dispatch to its exec implementation."
  (fn [node & [terminate?]]
    (cond (fn? node) :function
          :default (node :type))))

(defn- exec-action? [terminate?]
  (cond (nil? terminate?) true
        (not @terminate?) true
        :default false))

(defn- terminate [a]
  (swap! a (fn [_] (identity true))))

;;
;; Leafs
;;

(defmethod exec :function [f & [terminate?]]
  (if (exec-action? terminate?)
    (boolean (f)) false))

(defn action 
  "This node wraps a function call with blackboard as its argument."
  [function blackboard]
  {:type :action :function function :blackboard blackboard})

(defmethod exec :action
  [{function :function blackboard :blackboard} & [terminate?]]
  (if (exec-action? terminate?)
    (boolean ((resolve function) blackboard))
    false))

;;
;; Selectors
;;

(defn selector 
  "Tries to run all its children in sequence as soon as one succeeds 
   it also succeeds."
  [& children]
  {:type :selector :children children})

(defn non-deterministic-selector 
  "Same as selector, but shuffles all its children prior to execution."
  [& children]
  {:type :non-deterministic-selector :children children})

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
  {:type :sequence :children children})

(defn non-deterministic-sequence 
  "Same as sequence, but shuffles all its children prior to execution."
  [& children]
  {:type :non-deterministic-sequence :children children})

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
  [policy & children]
  "Concurrently executes all its children. If policy is :sequence, it acts as a sequence.
   If the policy is :selector it acts as a selector."
  {:type :parallel :policy policy :children children})

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
          (loop []
            (Thread/sleep 50)
            (cond (all-succeded? fs) true
                  (some-failed? fs)  (cancel fs self-terminate? false)
                  (not (exec-action? parent-terminate?)) (cancel fs self-terminate? false)
                  :default (recur)))

          (loop []
            (Thread/sleep 50)
            (cond (all-failed? fs) false
                  (some-succeded? fs) (cancel fs self-terminate? true)
                  (not (exec-action? parent-terminate?)) (cancel fs self-terminate? false)
                  :default (recur)))))
      false)))

;;
;; Decorators
;;

(defn forever
  "When its child task finishes, it runs it once more."
  [c]
  {:type :forever :children c})

(defmethod exec :forever [{children :children} & [terminate?]]
  (loop []
    (if (exec-action? terminate?)
      (do (exec children terminate?)
          (recur))
      false)))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  {:type :until-fail :children c})

(defmethod exec :until-fail [{children :children} & [terminate?]]
  (loop []
    (if (and (exec-action? terminate?)
             (exec children terminate?))
      (recur)
      true)))

(defn until-success
  "Runs its children until it returns true."
  [c]
  {:type :until-success :children c})

(defmethod exec :until-success [{children :children} & [terminate?]]
  (loop []
    (if (and (exec-action? terminate?)
             (not (exec children terminate?)))
      (recur)
      true)))

(defn limit 
  "Unless its children succeeds will keep running it at most i times."
  [c i]
  {:type :limit :children c :times i})

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
  {:type :inverter :children c})

(defmethod exec :inverter [{children :children} & [terminate?]]
  (not (exec children terminate?)))

(defn print-blackboard
  "Print the content of the blackboard"
  [b c]
  {:type :print-blackboard :blackboard b :children c} )

(defmethod exec :print-blackboard
  [{children :children blackboard :blackboard} & [terminate?]]
  (doseq [[key val] @blackboard]
    (println key " ==> " val))
  (exec children terminate?))

(defn print-string
  "Print a debug message."
  [s c]
  {:type :print-string :string s :children c})

(defmethod exec :print-string [{children :children string :string} & [terminate?]]
  (println string)
  (exec children terminate?))

(defn break-point
  "Insert a debug breakpoint."
  [s c]
  {:type :break-point :children c})

(defmethod exec :break-point [{children :children} & [terminate?]]
  (println "Press Enter to resume execution...")
  (.read System/in)
  (exec children terminate?))

(defn interrupter
  [w c p]
  "Lets its child node run normally. If the child returns a result,
   it passes that result on up the tree. But, if the child is still working,
   and watcher returns a result it will terminate the child and return the result of perform."
  {:type :interrupter :children c :watch w :perform p})

(defmethod exec :interrupter
  [{children :children watch :watch perform :perform} & [terminate?]]
  (if (exec-action? terminate?)
    (let [parent-terminate? terminate?
          terminate-children? (atom false)
          terminate-watch? (atom false)
          children (future (exec children terminate-children?))
          watch (future (exec watch terminate-watch?))]

      (loop []
        (Thread/sleep 50)
        (cond (future-done? children) (do (terminate terminate-watch?)
                                          (deref children))

              (and (future-done? watch)
                   (boolean @watch)) (do (terminate terminate-children?)
                                         (exec perform))

              (not (exec-action? parent-terminate?)) (do (terminate terminate-children?)
                                                         (terminate terminate-watch?)
                                                         (exec perform)
                                                         false)
              
              :default (recur))))
    false))

;;
;; Misc
;;

(defn- process-tree [node blackboard]
  (if (nil? (node :children))
    (do (if (and (= (:type node) :action)
                 (nil? (resolve (symbol (:function node)))))
          (throw (Exception. (str "Symbol not defined. " (:function node)))))
        (assoc node :blackboard blackboard))
    (let [children (:children node)]
      (assoc node :children (reduce (fn[h v]
                                      (conj h (process-tree v blackboard)))
                                    [] (filter #(not= (:status %) :disabled) (:children node)))))))

(defn load-tree
  ([file]
     (process-tree (read-string (slurp file)) (ref {})))
  ([file blackboard]
     (process-tree (read-string (slurp file)) blackboard)))

(defmacro from-blackboard 
  "A convenience macro to lookup bindings in the given blackboard."
  [blackboard bindings & body]
  `(let [~@(interleave bindings
		       (map cons 
			    (map keyword bindings)
			    (repeat (list (list 'deref `~blackboard)))))]
     ~@body))

(defmacro defaction
  "Define an action which looks up its bindings from blackboard."
  [name params & body]
  `(defn ~name [~'blackboard] 
     (from-blackboard ~'blackboard ~params
       ~@body)))
