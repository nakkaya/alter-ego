(ns #^{:author "Nurullah Akkaya"}
  alter-ego.node-types)

(derive ::composite ::type)
(derive ::decorator ::type)
(derive ::action ::type)

(derive ::selector ::composite)
(derive ::non-deterministic-selector ::composite)
(derive ::sequence ::composite)
(derive ::parallel-sequence ::composite)
(derive ::non-deterministic-sequence ::composite)

(derive ::forever ::decorator)
(derive ::until-fail ::decorator)
(derive ::until-success ::decorator)
(derive ::limit ::decorator)
(derive ::inverter ::decorator)
(derive ::print-blackboard ::decorator)
(derive ::print-string ::decorator)
(derive ::break-point ::decorator)
(derive ::interrupter ::composite)

(defmulti run 
  "Given a node dispatch to its run implementation."
  (fn [node & [terminate?]]
    (cond (fn? node) :function
          :default ((meta node) :type))))

(defn run-action? [terminate?]
  (cond (nil? terminate?) true
        (not @terminate?) true
        :default false))

(defn terminate [a]
  (swap! a (fn [_] (identity true))))

(defmulti append-child
  "Given nodes parent and child, dispatch to correct append-child 
   implementation."
  (fn [parent child] [((meta parent) :type) 
		      ((meta child) :type)]))
