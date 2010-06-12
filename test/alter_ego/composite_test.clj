(ns alter-ego.composite-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.composite] :reload-all)
  (:use [clojure.test]))

(defn inc-i [blackboard]
  (let [{i :i} @blackboard] 
    (dosync (alter blackboard assoc :i (inc i)))))

(deftest action-test
 (let [board (ref {:i 0})
       inc-i-action (action 'alter-ego.composite-test/inc-i board)]
   (is (= 1 (do (run inc-i-action) (:i @board))))
   (is (= 2 (do (run inc-i-action) (:i @board))))))

(defn door-open? [blackboard]
  (:door-open @blackboard))

(defn selector-tree-1 [blackboard]
  (selector 
   [(action 'alter-ego.composite-test/door-open? blackboard)
    (action 'alter-ego.composite-test/inc-i blackboard)]))

(defn selector-tree-2 [blackboard]
  (selector 
   [(action 'alter-ego.composite-test/inc-i blackboard)
    (action 'alter-ego.composite-test/door-open? blackboard)
    (action 'alter-ego.composite-test/inc-i blackboard)]))

(deftest selector-test
  (let [blackboard (ref {:door-open true :i 0})
	tree-1 (selector-tree-1 blackboard)
	tree-2 (selector-tree-2 blackboard)]
    (is (= 0  (do (run tree-1) (:i @blackboard))))
    (is (= 1  (do (run tree-2) (:i @blackboard))))))

(defn sequence-tree-1 [blackboard]
  (sequence
   [(action 'alter-ego.composite-test/door-open? blackboard)
    (action 'alter-ego.composite-test/inc-i blackboard)
    (action 'alter-ego.composite-test/inc-i blackboard)]))

(defn sequence-tree-2 [blackboard]
  (sequence
   [(action 'alter-ego.composite-test/inc-i blackboard)
    (action 'alter-ego.composite-test/door-open? blackboard)
    (action 'alter-ego.composite-test/inc-i blackboard)]))

(deftest sequence-test
  (let [blackboard (ref {:door-open true :i 0})
	tree-1 (sequence-tree-1 blackboard)
	blackboard2 (ref {:door-open false :i 0})
	tree-2 (sequence-tree-2 blackboard2)]
    (is (= 2  (do (run tree-1) (:i @blackboard))))
    (is (= 1  (do (run tree-2) (:i @blackboard2))))))

(defn to-room [blackboard]
  (dosync (alter blackboard assoc :to-room true)))

(defn to-door [blackboard]
  (dosync (alter blackboard assoc :to-door true)))

(defn open-door [blackboard]
  (dosync (alter blackboard assoc :open-door true)))

(defn sample-tree [blackboard]
  (selector
   [(sequence [(action 'alter-ego.composite-test/door-open? blackboard)
	       (action 'alter-ego.composite-test/to-room blackboard)])

    (sequence [(action 'alter-ego.composite-test/to-door blackboard)
	       (action 'alter-ego.composite-test/open-door blackboard)
	       (action 'alter-ego.composite-test/to-room blackboard)])]))

(deftest tree-test
  (let [blackboard (ref {:door-open true})
	tree-1 (sample-tree blackboard)
	blackboard2 (ref {:door-open false})
	tree-2 (sample-tree blackboard2)]
    (is (= true (do (run tree-1) (:to-room @blackboard))))
    (is (= true (do (run tree-2) (:to-door @blackboard2))))
    (is (= true (do (run tree-2) (:open-door @blackboard2))))
    (is (= true (do (run tree-2) (:to-room @blackboard2))))))
