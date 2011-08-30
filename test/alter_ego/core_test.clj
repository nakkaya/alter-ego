(ns alter-ego.core-test
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.core] :reload-all)
  (:use [alter-ego.sample-actions] :reload-all)
  (:use [clojure.test]))

(deftest action-test
  (let [board (ref {:i 0})]
    (is (= 1 (do (exec (inc-i-action board)) (:i @board))))
    (is (= true (exec (inc-i-action board))))
    (is (= 5 (do (exec (fn []
                         (dosync (alter board assoc :i 5)))) (:i @board))))
    (is (= true (exec (fn []
                        (dosync (alter board assoc :i 5))))))))

(defn selector-tree-1 [blackboard]
  (selector (door-open?-action blackboard)
            (inc-i-action blackboard)))

(defn selector-tree-2 [blackboard]
  (selector (inc-i-action blackboard)
	    (door-open?-action blackboard)
	    (inc-i-action blackboard)))

(defn selector-tree-3 [blackboard]
  (selector (door-open?-action blackboard)
            (inc-i-action blackboard)))

(deftest selector-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
	tree-1 (selector-tree-1 blackboard)
	tree-2 (selector-tree-2 blackboard)
	tree-3 (selector-tree-3 blackboard2)
	single (selector (inc-i-action blackboard2))]
    (is (= 0 (do (exec tree-1) (get-i blackboard))))
    (is (= 1 (do (exec tree-2) (get-i blackboard))))
    (is (= 1 (do (exec tree-3) (get-i blackboard2))))
    (is (= true (exec single)))
    (is (= 3 (do (exec tree-3) (get-i blackboard2))))))

(defn non-deterministic-selector-tree [blackboard]
  (non-deterministic-selector (inc-i-action blackboard)
                              (inc-i-action blackboard)))

(deftest non-deterministic-selector-test
  (let [blackboard (ref {:i 0})
	tree (non-deterministic-selector-tree blackboard)
	single (non-deterministic-selector (inc-i-action blackboard))]
    (is (= 1 (do (exec tree) (get-i blackboard))))
    (is (= true (exec tree)))
    (is (= 3 (do (exec single) (get-i blackboard))))
    (is (= true (exec single)))))

(defn sequence-tree-1 [blackboard]
  (sequence (door-open?-action blackboard)
            (inc-i-action blackboard)
            (inc-i-action blackboard)))

(defn sequence-tree-2 [blackboard]
  (sequence (inc-i-action blackboard)
            (door-open?-action blackboard)
            (inc-i-action blackboard)))

(defn sequence-tree-3 [bb]
  (let [f1 (fn [] (dosync (alter bb assoc :i 5)))
        f2 (fn [i] (dosync (alter bb assoc :i (+ i (:i @bb)))))]
    (sequence f1
              #(f2 5))))

(deftest sequence-test
  (let [blackboard (ref {:door-open true :i 0})
	blackboard2 (ref {:door-open false :i 0})
        blackboard3 (ref {:i 0})
	tree-1 (sequence-tree-1 blackboard)
	tree-2 (sequence-tree-2 blackboard2)
        tree-3 (sequence-tree-3 blackboard3)
	single (sequence (inc-i-action blackboard2))]
    (is (= 2  (do (exec tree-1) (get-i blackboard))))
    (is (= 1  (do (exec tree-2) (get-i blackboard2))))
    (is (= 2  (do (exec single) (get-i blackboard2))))
    (is (= 10  (do (exec tree-3) (get-i blackboard3))))))

(defn non-deterministic-sequence-tree [blackboard]
  (non-deterministic-sequence (inc-i-action blackboard)
                              (inc-i-action blackboard)))

(deftest non-deterministic-sequence-test
  (let [blackboard (ref {:i 0})
	tree (non-deterministic-sequence-tree blackboard)
	single (non-deterministic-sequence (inc-i-action blackboard))]
    (is (= 2 (do (exec tree) (get-i blackboard))))
    (is (= true (exec tree)))
    (is (= 5 (do (exec single) (get-i blackboard))))
    (is (= true (exec single)))))

(defn seq-return-tree [blackboard]
  (sequence (inc-i-action blackboard)
            (small?-action blackboard)))

(deftest seq-return-test
  (let [blackboard (ref {:i 0})
	blackboard2 (ref {:i 10})
	tree-1 (seq-return-tree blackboard)
	tree-2 (seq-return-tree blackboard2)
	single (sequence (inc-i-action blackboard2))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))
    (is (= true (exec single)))))

(deftest parallel-test
  (is (= false (exec (parallel :sequence
                               (sequence #(identity true))
                               (sequence #(identity true)))
                     (atom true))))
  (is (= 2 (let [a (atom 0)]
             (exec (parallel :sequence
                             (sequence #(swap! a inc)
                                       #(identity false))

                             (sequence #(swap! a inc)
                                       #(do (Thread/sleep 250) true)
                                       #(swap! a inc))))
             @a)))

  (is (= 3 (let [a (atom 0)]
             (exec (parallel :sequence
                             (sequence #(swap! a inc)
                                       #(identity true))

                             (sequence #(swap! a inc)
                                       #(do (Thread/sleep 250) true)
                                       #(swap! a inc))))
             @a)))
  
  (is (= true (exec (parallel :selector
                              (sequence #(identity true))
                              (sequence #(identity false))))))

  (is (= false (exec (parallel :selector
                               (sequence #(identity false))
                               (sequence #(identity false)))))))

(defn sample-tree [blackboard]
  (selector (sequence (door-open?-action blackboard)
                      (to-room-action blackboard))
            (sequence (to-door-action blackboard)
                      (open-door-action blackboard)
                      (to-room-action blackboard))))

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
  (until-fail (sequence (inc-i-action blackboard)
                        (small?-action blackboard))))

(deftest until-fail-test
  (let [blackboard (ref {:i 0})
	tree-1 (until-fail-tree blackboard)]
    (is (= 5 (do (exec tree-1) (:i @blackboard))))
    (is (= true (exec tree-1)))))

(defn until-success-tree [blackboard]
  (until-success (sequence (dec-i-action blackboard)
                           (small?-action blackboard))))

(deftest until-success-test
  (let [blackboard (ref {:i 10})
	tree-1 (until-success-tree blackboard)]
    (is (= 4 (do (exec tree-1) (:i @blackboard))))
    (is (= true (exec tree-1)))))

(defn limit-tree [blackboard]
  (limit (sequence (inc-i-action blackboard)
                   (small?-action blackboard)) 3))

(deftest limit-test
  (let [blackboard (ref {:i 6})
	tree (limit-tree blackboard)
	single (limit (inc-i-action blackboard) 3)]
    (is (= 9 (do (exec tree) (:i @blackboard))))
    (is (= false (exec tree)))
    (is (= true (exec single)))))

(deftest inverter-test
  (let [tree-1 (inverter (small?-action (ref {:i 6})))
	tree-2 (inverter (small?-action (ref {:i 0})))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))))

(deftest print-blackboard-test
  (let [bb1 (ref {:i 0})
	tree-1 (print-blackboard bb1 (small?-action bb1))
	bb2 (ref {:i 6})
	tree-2 (print-blackboard bb2 (small?-action bb2))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))
    (is (= ":i  ==>  6\n" (with-out-str (exec tree-2))))
    (is (= ":i  ==>  0\n" (with-out-str (exec tree-1))))))

(deftest print-string-test
  (let [tree-1 (print-string "1" (small?-action (ref {:i 0})))
	tree-2 (print-string "2" (small?-action (ref {:i 6})))]
    (is (= true (exec tree-1)))
    (is (= false (exec tree-2)))
    (is (= "2\n" (with-out-str (exec tree-2))))
    (is (= "1\n" (with-out-str (exec tree-1))))))

(deftest interrupter-test
  (is (= 2 (let [a (atom 0)]
             (exec
              (interrupter #(swap! a inc)
                           (sequence #(do (Thread/sleep 500) true)
                                     #(swap! a inc))
                           #(swap! a inc)))
             @a)))

  (is (= false (exec
                (interrupter #(do  true)
                             (sequence #(do (Thread/sleep 500) true))
                             #(identity false))))))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(def sample-saved-tree
  {:type :selector :name "Root"
   :children [{:type :sequence :name "Open Door"
               :children [{:type :action :name "Door Open?" :function 'alter-ego.sample-actions/door-open?}
                          {:type :action :name "Move" :function 'alter-ego.sample-actions/to-room}]}
              {:type :sequence :name "Fire Tree"
               :children [{:type :sequence :name "Until Dead"
                           :children [{:type :action :name "Move" :function 'move :status :disabled}
                                      {:type :action :name "Fire" :function 'alter-ego.sample-actions/to-room}]}]}

              {:type :sequence :name "Closed Door" :status :disabled
               :children [{:type :action :name "Move" :function 'move}
                          {:type :action :name "Open Door" :function 'open}]}]})

(with-private-fns [alter-ego.core [process-tree]]
  (deftest load-test
    (let [tree (process-tree sample-saved-tree (ref {}))
          ]
      (is (= 'alter-ego.sample-actions/door-open?
             (-> sample-saved-tree :children first :children first :function)))
      (is (= 'alter-ego.sample-actions/to-room
             (-> sample-saved-tree :children first :children second :function)))

      (is (= 'alter-ego.sample-actions/to-room
             (-> sample-saved-tree :children first :children second :function))))))

(deftest from-blackboard-test
  (let [blackboard (ref {:key1 :val1 :key2 99 :target [3 4]})] 
    (is (= :val1  (from-blackboard blackboard [key1] key1)))
    (is (= 99  (from-blackboard blackboard [key2] key2)))
    (is (= [99 [3  4]]  
	   (from-blackboard blackboard [key2 target] [key2 target])))))
