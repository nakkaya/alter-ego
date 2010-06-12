(ns alter-ego.composite
  (:refer-clojure :exclude [sequence]))

(derive ::action ::type)
(derive ::selector ::type)
(derive ::sequence ::type)
(defmulti run (fn [type] ((meta type) :type)))

(defn action [symbol blackboard]
  (with-meta {:symbol symbol :blackboard blackboard} {:type ::action}))

(defmethod run ::action [action]
  (let [{symbol :symbol blackboard :blackboard} action]
    ((resolve symbol) blackboard)))

(defn selector [children]
  (with-meta {:children children} {:type ::selector}))

(defn- select [children]
  (if-let[s (seq children)] 
    (if-not (run (first s))
      (select (rest s))
      true)))

(defmethod run ::selector [selector]
  (let [{children :children} selector]
    (select children)))

(defn sequence [children]
  (with-meta {:children children} {:type ::sequence}))

(defn- seq-run [children]
  (if-let[s (seq children)] 
    (if (run (first s))
      (seq-run (rest s))
      false)))

(defmethod run ::sequence [sequence]
  (let [{children :children} sequence]
    (seq-run children)))
