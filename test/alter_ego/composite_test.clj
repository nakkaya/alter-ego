(ns alter-ego.composite-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.sample-actions] :reload-all)
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [clojure.test]))

(deftest action-test
 (let [board (ref {:i 0})]
   (is (= 1 (do (run (inc-i-action board)) (:i @board))))
   (is (= true (run (inc-i-action board))))))

(defn selector-tree-1 [blackboard]
  (selector [(door-open?-action blackboard)
	     (inc-i-action blackboard)]))

(defn selector-tree-2 [blackboard]
  (selector [(inc-i-action blackboard)
	     (door-open?-action blackboard)
	     (inc-i-action blackboard)]))

(defn selector-tree-3 [blackboard]
  (selector [(door-open?-action blackboard)
	     (inc-i-action blackboard)]))

(deftest selector-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
	tree-1 (selector-tree-1 blackboard)
	tree-2 (selector-tree-2 blackboard)
	tree-3 (selector-tree-3 blackboard2)
	single (selector [(inc-i-action blackboard2)])]
    (is (= 0 (do (run tree-1) (get-i blackboard))))
    (is (= 1 (do (run tree-2) (get-i blackboard))))
    (is (= 1 (do (run tree-3) (get-i blackboard2))))
    (is (= true (run single)))
    (is (= 3 (do (run tree-3) (get-i blackboard2))))))

(defn sequence-tree-1 [blackboard]
  (sequence [(door-open?-action blackboard)
	     (inc-i-action blackboard)
	     (inc-i-action blackboard)]))

(defn sequence-tree-2 [blackboard]
  (sequence [(inc-i-action blackboard)
	     (door-open?-action blackboard)
	     (inc-i-action blackboard)]))

(deftest sequence-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
	tree-1 (sequence-tree-1 blackboard)
	tree-2 (sequence-tree-2 blackboard2)
	single (sequence [(inc-i-action blackboard2)])]
    (is (= 2  (do (run tree-1) (get-i blackboard))))
    (is (= 1  (do (run tree-2) (get-i blackboard2))))
    (is (= 2  (do (run single) (get-i blackboard2))))))

(defn seq-return-tree [blackboard]
  (sequence [(inc-i-action blackboard)
	     (small?-action blackboard)]))

(deftest seq-return-test
  (let [blackboard (ref {:i 0})
	blackboard2 (ref {:i 10})
	tree-1 (seq-return-tree blackboard)
	tree-2 (seq-return-tree blackboard2)
	single (sequence [(inc-i-action blackboard2)])]
    (is (= true (run tree-1)))
    (is (= false (run tree-2)))
    (is (= true (run single)))))

(defn sample-tree [blackboard]
  (selector [(sequence [(door-open?-action blackboard)
			(to-room-action blackboard)])
	     (sequence [(to-door-action blackboard)
			(open-door-action blackboard)
			(to-room-action blackboard)])]))

(deftest tree-test
  (let [blackboard (ref {:door-open true})
	blackboard2 (ref {:door-open false})
	tree-1 (sample-tree blackboard)
	tree-2 (sample-tree blackboard2)]
    (is (= true (do (run tree-1) (:to-room @blackboard))))
    (is (= true (do (run tree-2) (:to-door @blackboard2))))
    (is (= true (do (run tree-2) (:open-door @blackboard2))))
    (is (= true (do (run tree-2) (:to-room @blackboard2))))))
