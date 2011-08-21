(ns #^{:author "Nurullah Akkaya"}
  alter-ego.decorator
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all))

(defn forever
  "When its child task finishes, it runs it once more."
  [c]
  {:type :forever :children c})

(defmethod run :forever [{children :children} & [terminate?]]
  (loop []
    (if (run-action? terminate?)
      (do (run children terminate?)
          (recur))
      false)))

(defn until-fail 
  "Runs its children until it returns false."
  [c]
  {:type :until-fail :children c})

(defmethod run :until-fail [{children :children} & [terminate?]]
  (loop []
    (if (and (run-action? terminate?)
             (run children terminate?))
      (recur)
      true)))

(defn until-success
  "Runs its children until it returns true."
  [c]
  {:type :until-success :children c})

(defmethod run :until-success [{children :children} & [terminate?]]
  (loop []
    (if (and (run-action? terminate?)
             (not (run children terminate?)))
      (recur)
      true)))

(defn limit 
  "Unless its children succeeds will keep running it at most i times."
  [c i]
  {:type :limit :children c :times i})

(defmethod run :limit [{children :children times :times} & [terminate?]]
  (loop [i times]
    (if (and (pos? i)
             (run-action? terminate?))
      (if (not (run children terminate?)) 
        (recur (dec i))
        true)
      false)))

(defn inverter 
  "Inverts its childrens return value, succees becames failure and 
   vice versa."
  [c]
  {:type :inverter :children c})

(defmethod run :inverter [{children :children} & [terminate?]]
  (not (run children terminate?)))

(defn print-blackboard
  "Print the content of the blackboard"
  [b c]
  {:type :print-blackboard :blackboard b :children c} )

(defmethod run :print-blackboard
  [{children :children blackboard :blackboard} & [terminate?]]
  (doseq [[key val] @blackboard]
    (println key " ==> " val))
  (run children terminate?))

(defn print-string
  "Print a debug message."
  [s c]
  {:type :print-string :string s :children c})

(defmethod run :print-string [{children :children string :string} & [terminate?]]
  (println string)
  (run children terminate?))

(defn break-point
  "Insert a debug breakpoint."
  [s c]
  {:type :break-point :children c})

(defmethod run :break-point [{children :children} & [terminate?]]
  (println "Press Enter to resume execution...")
  (.read System/in)
  (run children terminate?))

(defn interrupter
  [w c p]
  "Lets its child node run normally. If the child returns a result,
   it passes that result on up the tree. But, if the child is still working,
   and watcher returns a result it will terminate the child and return the result of perform."
  {:type :interrupter :children c :watch w :perform p})

(defmethod run :interrupter
  [{children :children watch :watch perform :perform} & [terminate?]]
  (if (run-action? terminate?)
    (let [parent-terminate? terminate?
          terminate-children? (atom false)
          terminate-watch? (atom false)
          children (future (run children terminate-children?))
          watch (future (run watch terminate-watch?))]

      (loop []
        (Thread/sleep 50)
        (cond (not (run-action? parent-terminate?)) (do (terminate terminate-children?)
                                                        (run perform)
                                                        false)
              
              (future-done? children) (do (terminate terminate-watch?)
                                          (deref children))

              (and (future-done? watch)
                   (boolean @watch))  (do (terminate terminate-children?)
                                          (run perform))
                   :default (recur))))
    false))
