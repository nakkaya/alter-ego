(ns #^{:author "Nurullah Akkaya"}
  alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/until-fail}))

(defn- run-until-fail [children terminate?]
  (loop []
    (if (and (run-action? terminate?)
             (run children terminate?))
      (recur)
      true)))

(defmethod run :alter-ego.node-types/until-fail [{children :children} & [terminate?]]
  (run-until-fail children terminate?))

(defn until-success
  "Runs its children until it returns true."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/until-success}))

(defn- run-until-success [children terminate?]
  (loop []
    (if (and (run-action? terminate?)
             (not (run children terminate?)))
      (recur)
      true)))

(defmethod run :alter-ego.node-types/until-success [{children :children} & [terminate?]]
  (run-until-success children terminate?))

(defn limit 
  "Unless its children succeeds will keep running it at most i times."
  [c i]
  (with-meta {:children c :times i} {:type :alter-ego.node-types/limit}))

(defn- run-limit [children times terminate?]
  (if (and (pos? times)
           (run-action? terminate?))
    (if (not (run children terminate?)) 
      (run-limit children (dec times) terminate?)
      true)
    false))

(defmethod run :alter-ego.node-types/limit [{children :children times :times} & [terminate?]]
  (run-limit children times terminate?))

(defn inverter 
  "Inverts its childrens return value, succees becames failure and 
   vice versa."
  [c]
  (with-meta {:children c} {:type :alter-ego.node-types/inverter}))

(defmethod run :alter-ego.node-types/inverter [{children :children} & [terminate?]]
  (not (run children terminate?)))

(defn print-blackboard
  "Print the content of the blackboard"
  [b c]
  (with-meta {:blackboard b :children c} 
    {:type :alter-ego.node-types/print-blackboard}))

(defmethod run :alter-ego.node-types/print-blackboard
  [{children :children blackboard :blackboard} & [terminate?]]
  (doseq [[key val] @blackboard]
    (println key " ==> " val))
  (run children terminate?))

(defn print-string
  "Print a debug message."
  [s c]
  (with-meta {:string s :children c} 
    {:type :alter-ego.node-types/print-string}))

(defmethod run :alter-ego.node-types/print-string [{children :children string :string} & [terminate?]]
  (println string)
  (run children terminate?))

(defn break-point
  "Insert a debug breakpoint."
  [s c]
  (with-meta {:children c} 
    {:type :alter-ego.node-types/break-point}))

(defmethod run :alter-ego.node-types/break-point [{children :children} & [terminate?]]
  (println "Press Enter to resume execution...")
  (.read System/in)
  (run children terminate?))

(defn interrupter
  [w c p]
  (with-meta {:children c :watch w :perform p}
    {:type :alter-ego.node-types/interrupter}))

(defmethod run :alter-ego.node-types/interrupter
  [{children :children watch :watch perform :perform} & [terminate?]]
  (if (run-action? terminate?)
    (let [terminate? (if (nil? terminate?) (atom false) terminate?)
          terminate? (atom false)
          children (future (run children terminate?))
          watch (future (run watch terminate?))]

      (loop []
        (Thread/sleep 50)
        (cond (future-done? children)
              (deref children)

              (and (future-done? watch)
                   (boolean @watch))  (do (swap! terminate? (fn [_] (identity true)))
                                          (run perform))
                   :default (recur))))
    (run perform)))
