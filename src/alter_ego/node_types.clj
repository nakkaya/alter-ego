(ns alter-ego.node-types)

(derive ::composite ::type)
(derive ::decorator ::type)
(derive ::action ::type)

(derive ::selector ::composite)
(derive ::non-deterministic-selector ::composite)
(derive ::sequence ::composite)
(derive ::non-deterministic-sequence ::composite)

(derive ::until-fail ::decorator)

(defmulti run (fn [type] ((meta type) :type)))
