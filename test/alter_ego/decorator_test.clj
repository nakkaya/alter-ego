(ns alter-ego.decorator-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all)
  (:use [alter-ego.sample-actions] :reload-all)
  (:use [clojure.test]))

(defn until-fail-tree [blackboard]
  (until-fail (sequence [(inc-i-action blackboard)
			 (small?-action blackboard)])))

(deftest until-fail-test
  (let [blackboard (ref {:i 0})
	tree-1 (until-fail-tree blackboard)]
    (is (= 5 (do (run tree-1) (:i @blackboard))))
    (is (= true (run tree-1)))))

(defn limit-tree [blackboard]
  (limit (sequence [(inc-i-action blackboard)
		    (small?-action blackboard)]) 3))

(deftest limit-test
  (let [blackboard (ref {:i 6})
	tree (limit-tree blackboard)
	single (limit (inc-i-action blackboard) 3)]
    (is (= 9 (do (run tree) (:i @blackboard))))
    (is (= false (run tree)))
    (is (= true (run single)))))
