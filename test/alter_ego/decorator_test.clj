(ns alter-ego.decorator-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all)
  (:use [clojure.test]))

(defn inc-i [blackboard]
  (let [{i :i} @blackboard] 
    (dosync (alter blackboard assoc :i (inc i)))))

(defn small? [blackboard]
  (if (< (:i @blackboard) 5) true false))

(defn until-fail-tree [blackboard]
  (until-fail 
   (sequence [(action 'alter-ego.decorator-test/inc-i blackboard)
	      (action 'alter-ego.decorator-test/small? blackboard)])))

(deftest tree-test
  (let [blackboard (ref {:i 0})
	tree-1 (until-fail-tree blackboard)]
    (is (= 5 (do (run tree-1) (:i @blackboard))))))
