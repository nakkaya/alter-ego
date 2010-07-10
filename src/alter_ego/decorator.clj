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

(defn try-catch
  "Execute children on exception return false."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/try-catch}))

(defmethod run :alter-ego.node-types/try-catch [c]
  (let [{children :children} c]
    (try (run children) (catch Exception e false))))

(defn print-blackboard
  "Print the content of the blackboard"
  [b c]
  (with-meta {:blackboard b :children c} 
    {:type :alter-ego.node-types/print-blackboard}))

(defmethod run :alter-ego.node-types/print-blackboard [c]
  (let [{children :children blackboard :blackboard} c]
    (doseq [[key val] @blackboard]
      (println key " ==> " val))
    (run children)))

(defn print-string
  "Print a debug message."
  [s c]
  (with-meta {:string s :children c} 
    {:type :alter-ego.node-types/print-string}))

(defmethod run :alter-ego.node-types/print-string [n]
  (let [{children :children string :string} n]
    (println string)
    (run children)))

(defn break-point
  "Insert a debug breakpoint."
  [s c]
  (with-meta {:children c} 
    {:type :alter-ego.node-types/break-point}))

(defmethod run :alter-ego.node-types/break-point [n]
  (let [{children :children} n]
    (println "Press Enter to resume execution...")
    (.read System/in)
    (run children)))
