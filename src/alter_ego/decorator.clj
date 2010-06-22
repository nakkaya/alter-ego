(ns #^{:author "Nurullah Akkaya"}
  alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/until-fail}))

(defn- run-until [children]
  (if (run children)
    (run-until children) true))

(defmethod run :alter-ego.node-types/until-fail [d]
  (let [{children :children} d]
    (run-until children)))

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
