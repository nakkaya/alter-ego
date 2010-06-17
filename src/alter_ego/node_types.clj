(ns alter-ego.node-types)

(derive ::composite ::type)
(derive ::decorator ::type)
(derive ::action ::type)

(derive ::selector ::composite)
(derive ::non-deterministic-selector ::composite)
(derive ::sequence ::composite)
(derive ::non-deterministic-sequence ::composite)

(derive ::until-fail ::decorator)
(derive ::limit ::decorator)
(derive ::inverter ::decorator)

(defmulti run (fn [type] ((meta type) :type)))
(defmulti append-child (fn [parent child] [((meta parent) :type) 
					   ((meta child) :type)]))
