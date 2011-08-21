(ns #^{:author "Nurullah Akkaya"}
  alter-ego.node-types)

(defmulti run 
  "Given a node dispatch to its run implementation."
  (fn [node & [terminate?]]
    (cond (fn? node) :function
          :default (node :type))))

(defn run-action? [terminate?]
  (cond (nil? terminate?) true
        (not @terminate?) true
        :default false))

(defn terminate [a]
  (swap! a (fn [_] (identity true))))