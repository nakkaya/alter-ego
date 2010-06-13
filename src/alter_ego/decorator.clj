(ns alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all))

(defn until-fail [c]
  (with-meta {:children c} {:type ::until-fail}))

(defn- run-until [children]
  (if (run children)
    (run-until children) true))

(defmethod run ::until-fail [d]
  (let [{children :children} d]
    (run-until children)))

(defn limit [c i]
  (with-meta {:children c :times i} {:type ::limit}))

(defn- run-limit [children times]
  (if (pos? times)
    (if-not (run children)
      (run-limit children (dec times))
      true)
    false))

(defmethod run ::limit [d]
  (let [{children :children times :times} d]
    (run-limit children times)))

(defn inverter [c]
  (with-meta {:children c} {:type ::inverter}))

(defmethod run ::inverter [c]
  (let [{children :children} c]
    (not (run children))))
