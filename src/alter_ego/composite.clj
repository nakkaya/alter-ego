(ns alter-ego.composite
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all))

(defn action [symbol blackboard]
  (with-meta {:symbol symbol :blackboard blackboard} {:type ::action}))

(defmethod run ::action [action]
  (let [{symbol :symbol blackboard :blackboard} action]
    (boolean ((resolve symbol) blackboard))))

(defn selector [children]
  (with-meta {:children children} {:type ::selector}))

(defn- select [children]
  (if-let[s (seq children)] 
    (if-not (run (first s))
      (select (rest s))
      true)))

(defmethod run ::selector [selector]
  (let [{children :children} selector]
    (if (not (true? (select children))) false true)))

(defn sequence [children]
  (with-meta {:children children} {:type ::sequence}))

(defn- seq-run [children]
  (if-let[s (seq children)] 
    (if (run (first s))
      (seq-run (rest s)) false)))

(defmethod run ::sequence [sequence]
  (let [{children :children} sequence]
    (if (not (false? (seq-run children))) true false)))
