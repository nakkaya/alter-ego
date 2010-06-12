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
