(ns #^{:author "Nurullah Akkaya"}
  alter-ego.node-types)

(derive ::composite ::type)
(derive ::decorator ::type)
(derive ::action ::type)

(derive ::selector ::composite)
(derive ::non-deterministic-selector ::composite)
(derive ::sequence ::composite)
(derive ::non-deterministic-sequence ::composite)

(derive ::until-fail ::decorator)
(derive ::until-success ::decorator)
(derive ::limit ::decorator)
(derive ::inverter ::decorator)
(derive ::try-catch ::decorator)
(derive ::print-blackboard ::decorator)

(defmulti run 
  "Given a node dispatch to its run implementation."
  (fn [type] ((meta type) :type)))

(defmulti append-child
  "Given nodes parent and child, dispatch to correct append-child 
   implementation."
  (fn [parent child] [((meta parent) :type) 
		      ((meta child) :type)]))
