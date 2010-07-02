(ns alter-ego.core-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.sample-actions] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all)
  (:use [alter-ego.core] :reload-all)
  (:use [clojure.test]))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(def sample-tree
     [{:type :selector :name "Root"}
      [{:type :sequence :name "Open Door"}
       [{:type :action :name "Door Open?" 
	 :function 'alter-ego.sample-actions/door-open?}]
       [{:type :action :name "Move" 
	 :function 'alter-ego.sample-actions/to-room}]]
      [{:type :sequence :name "Closed Door"}
       [{:type :action :name "Move" 
	 :function 'alter-ego.sample-actions/to-door}]
       [{:type :action :name "Open Door" 
	 :function 'alter-ego.sample-actions/open-door}]
       [{:type :until-fail :name "Until Fail"}
	[{:type :action :name "Move" 
	  :function 'alter-ego.sample-actions/to-room}]]]

      [{:type :sequence :name "Fire Tree"}
       [{:type :sequence :name "Until Dead"}
	[{:type :action :name "Move" :function 'move :status :disabled}]
	[{:type :action :name "Fire" 
	  :function 'alter-ego.sample-actions/to-room}]]]

      [{:type :sequence :name "Closed Door" :status :disabled}
       [{:type :action :name "Move" :function 'move}]
       [{:type :action :name "Open Door" :function 'open}]
       [{:type :until-fail :name "Until Fail"}
	[{:type :action :name "Move" :function 'move}]]]])

(with-private-fns [alter-ego.core [node]]
  (deftest load-test
    (let [blackboard (ref {})
	  tree (load-tree (node (first sample-tree) blackboard)
			  (rest sample-tree) blackboard)]
      (is (= 'alter-ego.sample-actions/door-open?
	     (:symbol (first (:children (first (:children tree)))))))
      (is (= 'alter-ego.sample-actions/to-room
	     (:symbol (second (:children (first (:children tree)))))))
      (is (= 'alter-ego.sample-actions/to-door
	     (:symbol (first (:children (second (:children tree)))))))
      (is (= 'alter-ego.sample-actions/open-door
	     (:symbol (second (:children (second (:children tree)))))))
      (is (= 3 (count (:children tree))))
      (is (= 1 (count (:children (nth (:children tree) 2))))))))

(deftest from-blackboard-test
  (let [blackboard (ref {:key1 :val1 :key2 99 :target [3 4]})] 
    (is (= :val1  (from-blackboard blackboard [key1] key1)))
    (is (= 99  (from-blackboard blackboard [key2] key2)))
    (is (= [99 [3  4]]  
	   (from-blackboard blackboard [key2 target] [key2 target])))))
