(ns alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.composite] :reload-all))

(derive ::until-fail ::type)

(defn until-fail [c]
  (with-meta {:children c} {:type ::until-fail}))

(defn- run-until [children]
  (if (run children)
    (run-until children) true))

(defmethod run ::until-fail [d]
  (let [{children :children} d]
    (run-until children)))
