(ns alter-ego.core-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all)
  (:use [alter-ego.core] :reload-all)
  (:use [clojure.test]))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(def sample-tree
     [{:type :selector, :name "Root"}
      [{:type :sequence, :name "Open Door"}
       [{:type :action, :name "Door Open?", :action 'door-open?}]
       [{:type :action, :name "Move", :action 'move}]]
      [{:type :sequence, :name "Closed Door"}
       [{:type :action, :name "Move", :action 'move}]
       [{:type :action, :name "Open Door", :action 'open}]
       [{:type :until-fail, :name "Until Fail"}
	[{:type :action, :name "Move", :action 'move}]]]])

(with-private-fns [alter-ego.core [node]]
  (deftest load-test
    (let [blackboard (ref {})
	  tree (load-tree (node (first sample-tree) blackboard)
			  (rest sample-tree) blackboard)]
      (is (= 'door-open? 
	     (:symbol (first (:children (first (:children tree)))))))
      (is (= 'move 
	     (:symbol (second (:children (first (:children tree)))))))
      (is (= 'move 
	     (:symbol (first (:children (second (:children tree)))))))
      (is (= 'open
	     (:symbol (second (:children (second (:children tree))))))))))
