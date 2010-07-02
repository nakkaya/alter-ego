(ns #^{:author "Nurullah Akkaya"}
  alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/until-fail}))

(defn- run-until-fail [children]
  (if (run children)
    (run-until-fail children) true))

(defmethod run :alter-ego.node-types/until-fail [d]
  (let [{children :children} d]
    (run-until-fail children)))

(defn until-success
  "Runs its children until it returns true."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/until-success}))

(defn- run-until-success [children]
  (if (not (run children))
    (run-until-success children) true))

(defmethod run :alter-ego.node-types/until-success [d]
  (let [{children :children} d]
    (run-until-success children)))

(defn limit 
  "Unless its children succeeds will keep running it at most i times."
  [c i]
  (with-meta {:children c :times i} {:type :alter-ego.node-types/limit}))

(defn- run-limit [children times]
  (if (pos? times)
    (if-not (run children)
      (run-limit children (dec times))
      true)
    false))

(defmethod run :alter-ego.node-types/limit [d]
  (let [{children :children times :times} d]
    (run-limit children times)))

(defn inverter 
  "Inverts its childrens return value, succees becames failure and 
   vice versa."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/inverter}))

(defmethod run :alter-ego.node-types/inverter [c]
  (let [{children :children} c]
    (not (run children))))
