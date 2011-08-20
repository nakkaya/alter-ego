(ns #^{:author "Nurullah Akkaya"}
  alter-ego.composite
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all))

;;
;; Leafs
;;

(defmethod run :function [f & [terminate?]]
  (if (run-action? terminate?)
    (boolean (f)) false))

(defn action 
  "This node wraps a function call with blackboard as its argument."
  [symbol blackboard]
  (with-meta {:symbol symbol :blackboard blackboard} 
    {:type :alter-ego.node-types/action}))

(defmethod run :alter-ego.node-types/action
  [{symbol :symbol blackboard :blackboard} & [terminate?]]
  (if (run-action? terminate?)
    (boolean ((resolve symbol) blackboard))
    false))

;;
;; Selectors
;;

(defn selector 
  "Tries to run all its children in sequence as soon as one succeeds 
   it also succeeds."
  [& children]
  (with-meta {:children children} {:type :alter-ego.node-types/selector}))

(defn non-deterministic-selector 
  "Same as selector, but shuffles all its children prior to execution."
  [& children]
  (with-meta {:children children} 
    {:type :alter-ego.node-types/non-deterministic-selector}))

(defn- select [children terminate?]
  (if (run-action? terminate?)
    (if-let[s (seq children)] 
      (if-not (run (first s) terminate?)
        (recur (rest s) terminate?)
        true))
    false))

(defmethod run :alter-ego.node-types/selector [{children :children} & [terminate?]]
  (if (not (true? (select children terminate?))) false true))

(defmethod run :alter-ego.node-types/non-deterministic-selector [{children :children} & [terminate?]]
  (if (not (true? (select (shuffle children) terminate?))) false true))

;;
;; Sequences
;;

(defn sequence 
  "Runs all of its children in sequential order. If one of them fails, 
   it also fails. Once all of them succeeds, it also succeeds."
  [& children]
  (with-meta {:children children} {:type :alter-ego.node-types/sequence}))

(defn non-deterministic-sequence 
  "Same as sequence, but shuffles all its children prior to execution."
  [& children]
  (with-meta {:children children} {:type :alter-ego.node-types/non-deterministic-sequence}))

(defn- seq-run [children terminate?]
  (if (run-action? terminate?)
    (if-let [s (seq children)]
      (if (run (first s) terminate?)
        (recur (rest s) terminate?)
        false)
      true)
    false))

(defmethod run :alter-ego.node-types/sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run children terminate?))) true false))

(defmethod run :alter-ego.node-types/non-deterministic-sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run (shuffle children) terminate?))) true false))
