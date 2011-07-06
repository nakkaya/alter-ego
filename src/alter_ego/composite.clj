(ns #^{:author "Nurullah Akkaya"}
  alter-ego.composite
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all))

(defmethod run :function [f]
           (boolean (f)))

(defn action 
  "This node wraps a function call with blackboard as its argument."
  [symbol blackboard]
  (with-meta {:symbol symbol :blackboard blackboard} 
    {:type :alter-ego.node-types/action}))

(defmethod run :alter-ego.node-types/action [action]
  (let [{symbol :symbol blackboard :blackboard} action]
    (boolean ((resolve symbol) blackboard))))

(defn selector 
  "Tries to run all its children in sequence as soon as one succeeds 
   it also succeeds."
  [& children]
  (with-meta {:children children} {:type :alter-ego.node-types/selector}))

(defn- select [children]
  (if-let[s (seq children)] 
    (if-not (run (first s))
      (select (rest s))
      true)))

(defmethod run :alter-ego.node-types/selector [selector]
  (let [{children :children} selector]
    (if (not (true? (select children))) false true)))

(defn non-deterministic-selector 
  "Same as selector, but shuffles all its children prior to execution."
  [& children]
  (with-meta {:children children} 
    {:type :alter-ego.node-types/non-deterministic-selector}))

(defmethod run :alter-ego.node-types/non-deterministic-selector [selector]
  (let [{children :children} selector]
    (if (not (true? (select (shuffle children)))) false true)))

(defn sequence 
  "Runs all of its children in sequential order. If one of them fails, 
   it also fails. Once all of them succeeds, it also succeeds."
  [& children]
  (with-meta {:children children} {:type :alter-ego.node-types/sequence}))

(defn- seq-run [children]
  (if-let[s (seq children)] 
    (if (run (first s))
      (seq-run (rest s)) false)))

(defmethod run :alter-ego.node-types/sequence [sequence]
  (let [{children :children} sequence]
    (if (not (false? (seq-run children))) true false)))

(defn non-deterministic-sequence 
  "Same as sequence, but shuffles all its children prior to execution."
  [& children]
  (with-meta {:children children} 
    {:type :alter-ego.node-types/non-deterministic-sequence}))

(defmethod run :alter-ego.node-types/non-deterministic-sequence [sequence]
  (let [{children :children} sequence]
    (if (not (false? (seq-run (shuffle children)))) true false)))
