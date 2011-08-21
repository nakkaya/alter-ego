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
  [symbol blackboard]
  {:type :action :symbol symbol :blackboard blackboard})

(defmethod exec :action
  [{symbol :symbol blackboard :blackboard} & [terminate?]]
  (if (exec-action? terminate?)
    (boolean ((resolve symbol) blackboard))
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

(defn parallel-sequence
  [& children]
  "Concurrently executes all its children. Parallel fails if one child fails;
   if all succeed, then the parallel succeed."
  {:type :parallel-sequence :children children})

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

(defn- all-futures-succeded? [fs]
  (and (every? true? (map future-done? fs))
       (every? true? (map deref fs))))

(defn- any-futures-failed? [fs]
  (some false? (map deref (filter future-done? fs))))

(defn- terminate-and-return [fs atom return]
  (terminate atom)
  (doall (map deref fs))
  return)

(defmethod exec :parallel-sequence [{children :children} & [terminate?]]
  (if (exec-action? terminate?)
    (let [parent-terminate? terminate?
          self-terminate? (atom false)
          fs (map #(future (exec % self-terminate?)) children)]
      (loop []
        (Thread/sleep 50)
        (cond (not (exec-action? parent-terminate?)) (terminate-and-return fs self-terminate? false)
              (all-futures-succeded? fs) true
              (any-futures-failed? fs)  (terminate-and-return fs self-terminate? false)
              :default (recur))))
    false))

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
        (cond (not (exec-action? parent-terminate?)) (do (terminate terminate-children?)
                                                         (exec perform)
                                                         false)
              
              (future-done? children) (do (terminate terminate-watch?)
                                          (deref children))

              (and (future-done? watch)
                   (boolean @watch))  (do (terminate terminate-children?)
                                          (exec perform))
                   :default (recur))))
    false))

;;
;; Misc
;;

(defn- node [n blackboard]
  (let [{t :type func :function string :string} n]
    (cond (= t :action) (if (nil? (resolve (symbol func)))
			  (throw (Exception. "Symbol not defined."))
			  (action (symbol func) blackboard))
	  (= t :selector) (selector)
	  (= t :non-deterministic-selector) (non-deterministic-selector)
	  (= t :sequence) (sequence)
	  (= t :non-deterministic-sequence) (non-deterministic-sequence)
	  (= t :until-fail) (until-fail nil)
	  (= t :until-success) (until-success nil)
	  (= t :limit) (limit nil)
	  (= t :inverter) (inverter nil)
	  (= t :print-blackboard) (print-blackboard blackboard nil)
	  (= t :print-string) (print-string string nil)
	  (= t :break-point) (break-point nil)
	  :default (throw (Exception. "Unknown node type.")))))

(defn- append-child [p c]
  (assoc p :children (reverse (conj (reverse (:children p)) c))))

(defn load-tree 
  "Load tree definition."
  ([file]
     (load-tree file (ref {})))
  ([file blackboard]
     (let [tree (read-string (slurp file))] 
       (load-tree (node (first tree) blackboard) (rest tree) blackboard)))
  ([parent children blackboard]
     (reduce (fn[h v]
	       (if (> (count v) 1)
	         (let [p (node (first v) blackboard)
	               c (rest v)]
	           (if (nil? (:status (first v)))
	             (append-child h (load-tree p c blackboard))
	             h))
	         (if (nil? (:status (first v)))
	           (append-child h (node (first v) blackboard))
	           h)))
	     parent children)))

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
