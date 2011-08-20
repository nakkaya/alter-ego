(ns alter-ego.decorator-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all)
  (:use [alter-ego.sample-actions] :reload-all)
  (:use [clojure.test]))

(defn until-fail-tree [blackboard]
  (until-fail (sequence (inc-i-action blackboard)
                        (small?-action blackboard))))

(deftest until-fail-test
  (let [blackboard (ref {:i 0})
	tree-1 (until-fail-tree blackboard)]
    (is (= 5 (do (run tree-1) (:i @blackboard))))
    (is (= true (run tree-1)))))

(defn until-success-tree [blackboard]
  (until-success (sequence (dec-i-action blackboard)
                           (small?-action blackboard))))

(deftest until-success-test
  (let [blackboard (ref {:i 10})
	tree-1 (until-success-tree blackboard)]
    (is (= 4 (do (run tree-1) (:i @blackboard))))
    (is (= true (run tree-1)))))

(defn limit-tree [blackboard]
  (limit (sequence (inc-i-action blackboard)
                   (small?-action blackboard)) 3))

(deftest limit-test
  (let [blackboard (ref {:i 6})
	tree (limit-tree blackboard)
	single (limit (inc-i-action blackboard) 3)]
    (is (= 9 (do (run tree) (:i @blackboard))))
    (is (= false (run tree)))
    (is (= true (run single)))))

(deftest inverter-test
  (let [tree-1 (inverter (small?-action (ref {:i 6})))
	tree-2 (inverter (small?-action (ref {:i 0})))]
    (is (= true (run tree-1)))
    (is (= false (run tree-2)))))

(deftest print-blackboard-test
  (let [bb1 (ref {:i 0})
	tree-1 (print-blackboard bb1 (small?-action bb1))
	bb2 (ref {:i 6})
	tree-2 (print-blackboard bb2 (small?-action bb2))]
    (is (= true (run tree-1)))
    (is (= false (run tree-2)))
    (is (= ":i  ==>  6\n" (with-out-str (run tree-2))))
    (is (= ":i  ==>  0\n" (with-out-str (run tree-1))))))

(deftest print-string-test
  (let [tree-1 (print-string "1" (small?-action (ref {:i 0})))
	tree-2 (print-string "2" (small?-action (ref {:i 6})))]
    (is (= true (run tree-1)))
    (is (= false (run tree-2)))
    (is (= "2\n" (with-out-str (run tree-2))))
    (is (= "1\n" (with-out-str (run tree-1))))))
