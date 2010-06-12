(ns alter-ego.node-types)

(derive ::action ::type)
(derive ::selector ::type)
(derive ::sequence ::type)
(derive ::until-fail ::type)

(defmulti run (fn [type] ((meta type) :type)))
