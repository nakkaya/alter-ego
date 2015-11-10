(ns alter-ego.core-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.core] :reload-all)
  (:use [clojure.test]))

(deftest action-test
  (let [board (atom 0)]
    (is (= 1 (do (exec (action (swap! board inc))) @board)))
    (is (= true (exec (action (swap! board inc)))))))

(deftest throttled-action-test
  (let [board (atom 0)
        inc-action (throttled-action 1 :second (swap! board inc))
        now (System/currentTimeMillis)]
    (is (= 1 (do (exec inc-action) @board)))
    (is (= 2 (do (exec inc-action) @board)))
    (is (= 3 (do (exec inc-action) @board)))
    (is (>= (- (System/currentTimeMillis) now) 2000))))

(defn inc-i [blackboard]
  (action (dosync (alter blackboard assoc :i (inc (:i @blackboard))))))

(defn dec-i [blackboard]
  (action (dosync (alter blackboard assoc :i (dec (:i @blackboard))))))

(defn get-i [blackboard]
  (:i @blackboard))

(defn small? [blackboard]
  (action (if (< (:i @blackboard) 5) true false)))

(defn door-open? [blackboard]
  (action (:door-open @blackboard)))

(defn to-room [blackboard]
  (action (dosync (alter blackboard assoc :to-room true))))

(defn to-door [blackboard]
  (action (dosync (alter blackboard assoc :to-door true))))

(defn open-door [blackboard]
  (action (dosync (alter blackboard assoc :open-door true))))

(defn selector-tree-1 [blackboard]
  (selector (door-open? blackboard)
            (inc-i blackboard)))

(defn selector-tree-2 [blackboard]
  (selector (inc-i blackboard)
	    ;;(door-open? blackboard)
	    ;;(inc-i blackboard)
            ))

(defn selector-tree-3 [blackboard]
  (selector (door-open? blackboard)
            (inc-i blackboard)))

(deftest selector-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
	tree-1 (selector-tree-1 blackboard)
	tree-2 (selector-tree-2 blackboard)
	tree-3 (selector-tree-3 blackboard2)
	single (selector (inc-i blackboard2))]
    (is (= 0 (do (exec tree-1) (get-i blackboard))))
    (is (= 1 (do (exec tree-2) (get-i blackboard))))
    (is (= 1 (do (exec tree-3) (get-i blackboard2))))
    (is (= true (exec single)))
    (is (= 3 (do (exec tree-3) (get-i blackboard2))))))

(defn non-deterministic-selector-tree [blackboard]
  (non-deterministic-selector (inc-i blackboard)
                              (inc-i blackboard)))

(deftest non-deterministic-selector-test
  (let [blackboard (ref {:i 0})
	tree (non-deterministic-selector-tree blackboard)
	single (non-deterministic-selector (inc-i blackboard))]
    (is (= 1 (do (exec tree) (get-i blackboard))))
    (is (= true (exec tree)))
    (is (= 3 (do (exec single) (get-i blackboard))))
    (is (= true (exec single)))))

(defn sequence-tree-1 [blackboard]
  (sequence (door-open? blackboard)
            (inc-i blackboard)
            (inc-i blackboard)))

(defn sequence-tree-2 [blackboard]
  (sequence (inc-i blackboard)
            (door-open? blackboard)
            (inc-i blackboard)))

(defn sequence-tree-3 [bb]
  (let [f1 (action (dosync (alter bb assoc :i 5)))
        f2 (fn [i] (dosync (alter bb assoc :i (+ i (:i @bb)))))]
    (sequence f1
              (action (f2 5)))))

(deftest sequence-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
        blackboard3 (ref {:i 0})
	tree-1 (sequence-tree-1 blackboard)
	tree-2 (sequence-tree-2 blackboard2)
        tree-3 (sequence-tree-3 blackboard3)
	single (sequence (inc-i blackboard2))]
    (is (= 2  (do (exec tree-1) (get-i blackboard))))
    (is (= 1  (do (exec tree-2) (get-i blackboard2))))
    (is (= 2  (do (exec single) (get-i blackboard2))))
    (is (= 10  (do (exec tree-3) (get-i blackboard3))))))

(defn non-deterministic-sequence-tree [blackboard]
  (non-deterministic-sequence (inc-i blackboard)
                              (inc-i blackboard)))

(deftest non-deterministic-sequence-test
  (let [blackboard (ref {:i 0})
	tree (non-deterministic-sequence-tree blackboard)
	single (non-deterministic-sequence (inc-i blackboard))]
    (is (= 2 (do (exec tree) (get-i blackboard))))
    (is (= true (exec tree)))
    (is (= 5 (do (exec single) (get-i blackboard))))
    (is (= true (exec single)))))

(defn seq-return-tree [blackboard]
  (sequence (inc-i blackboard)
            (small? blackboard)))

(deftest seq-return-test
  (let [blackboard (ref {:i 0})
	blackboard2 (ref {:i 10})
	tree-1 (seq-return-tree blackboard)
	tree-2 (seq-return-tree blackboard2)
	single (sequence (inc-i blackboard2))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))
    (is (= true (exec single)))))

(deftest parallel-test
  (is (= false (exec (parallel :sequence
                               (sequence (action true))
                               (sequence (action true)))
                     (atom true))))
  (is (= 2 (let [a (atom 0)]
             (exec (parallel :sequence
                             (sequence (action (swap! a inc))
                                       (action false))

                             (sequence (action (swap! a inc))
                                       (action (Thread/sleep 250) true)
                                       (action (swap! a inc)))))
             @a)))

  (is (= 3 (let [a (atom 0)]
             (exec (parallel :sequence
                             (sequence (action (swap! a inc))
                                       (action true))

                             (sequence (action (swap! a inc))
                                       (action (Thread/sleep 250) true)
                                       (action (swap! a inc)))))
             @a)))
  
  (is (= true (exec (parallel :selector
                              (sequence (action true))
                              (sequence (action false))))))

  (is (= false (exec (parallel :selector
                               (sequence (action false))
                               (sequence (action false)))))))

(defn sample-tree [blackboard]
  (selector (sequence (door-open? blackboard)
                      (to-room blackboard))
            (sequence (to-door blackboard)
                      (open-door blackboard)
                      (to-room blackboard))))

(deftest tree-test
  (let [blackboard (ref {:door-open true})
	blackboard2 (ref {:door-open false})
	tree-1 (sample-tree blackboard)
	tree-2 (sample-tree blackboard2)]
    (is (= true (do (exec tree-1) (:to-room @blackboard))))
    (is (= true (do (exec tree-2) (:to-door @blackboard2))))
    (is (= true (do (exec tree-2) (:open-door @blackboard2))))
    (is (= true (do (exec tree-2) (:to-room @blackboard2))))))

(defn until-fail-tree [blackboard]
  (until-fail (sequence (inc-i blackboard)
                        (small? blackboard))))

(deftest until-fail-test
  (let [blackboard (ref {:i 0})
	tree-1 (until-fail-tree blackboard)]
    (is (= 5 (do (exec tree-1) (:i @blackboard))))
    (is (= true (exec tree-1)))))

(defn until-success-tree [blackboard]
  (until-success (sequence (dec-i blackboard)
                           (small? blackboard))))

(deftest until-success-test
  (let [blackboard (ref {:i 10})
	tree-1 (until-success-tree blackboard)]
    (is (= 4 (do (exec tree-1) (:i @blackboard))))
    (is (= true (exec tree-1)))))

(defn limit-tree [blackboard]
  (limit 3 (sequence (inc-i blackboard)
                     (small? blackboard))))

(deftest limit-test
  (let [blackboard (ref {:i 6})
	tree (limit-tree blackboard)
	single (limit 3 (inc-i blackboard))]
    (is (= 9 (do (exec tree) (:i @blackboard))))
    (is (= false (exec tree)))
    (is (= true (exec single)))))

(deftest inverter-test
  (let [tree-1 (inverter (small? (ref {:i 6})))
	tree-2 (inverter (small? (ref {:i 0})))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))))

(deftest interrupter-test
  (is (= 2 (let [a (atom 0)]
             (exec
              (interrupter (action (swap! a inc))
                           (sequence (action (Thread/sleep 500) true)
                                     (action (swap! a inc)))
                           (action (swap! a inc))))
             @a)))

  (is (= false (exec
                (interrupter (action true)
                             (sequence (action (Thread/sleep 500) true))
                             (action false))))))

(deftest interrupter-test
  (is (= 1 (let [a (atom 0)]
             (exec-repl (action (throw (Exception. "Test Exception")))
                        (action (swap! a inc)))
             @a)))

  (is (= 0 (let [a (atom 0)]
             (exec-repl (action )
                        (action (swap! a inc)))
             @a)))

  (is (= 1 (let [a (atom 0)]
             (exec-repl (action (swap! a inc))
                        (action (swap! a inc)))
             @a))))
