(ns #^{:author "Nurullah Akkaya"}
  alter-ego.composite
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all))

;;
;; Leafs
;;

(defmethod run :function [f & [terminate?]]
  (if (run-action? terminate?)
    (boolean (f)) false))

(defn action 
  "This node wraps a function call with blackboard as its argument."
  [symbol blackboard]
  {:type :action :symbol symbol :blackboard blackboard})

(defmethod run :action
  [{symbol :symbol blackboard :blackboard} & [terminate?]]
  (if (run-action? terminate?)
    (boolean ((resolve symbol) blackboard))
    false))

;;
;; Selectors
;;

(defn selector 
  "Tries to run all its children in sequence as soon as one succeeds 
   it also succeeds."
  [& children]
  {:type :selector :children children})

(defn non-deterministic-selector 
  "Same as selector, but shuffles all its children prior to execution."
  [& children]
  {:type :non-deterministic-selector :children children})

(defn- select [children terminate?]
  (if (run-action? terminate?)
    (if-let[s (seq children)] 
      (if-not (run (first s) terminate?)
        (recur (rest s) terminate?)
        true)
      false)
    false))

(defmethod run :selector [{children :children} & [terminate?]]
  (if (not (true? (select children terminate?))) false true))

(defmethod run :non-deterministic-selector [{children :children} & [terminate?]]
  (if (not (true? (select (shuffle children) terminate?))) false true))

;;
;; Sequences
;;

(defn sequence 
  "Runs all of its children in sequential order. If one of them fails, 
   it also fails. Once all of them succeeds, it also succeeds."
  [& children]
  {:type :sequence :children children})

(defn non-deterministic-sequence 
  "Same as sequence, but shuffles all its children prior to execution."
  [& children]
  {:type :non-deterministic-sequence :children children})

(defn parallel-sequence
  [& children]
  "Concurrently executes all its children. Parallel fails if one child fails;
   if all succeed, then the parallel succeed."
  {:type :parallel-sequence :children children})

(defn- seq-run [children terminate?]
  (if (run-action? terminate?)
    (if-let [s (seq children)]
      (if (run (first s) terminate?)
        (recur (rest s) terminate?)
        false)
      true)
    false))

(defmethod run :sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run children terminate?))) true false))

(defmethod run :non-deterministic-sequence [{children :children} & [terminate?]]
  (if (not (false? (seq-run (shuffle children) terminate?))) true false))

(defn- all-futures-succeded? [fs]
  (and (every? true? (map future-done? fs))
       (every? true? (map deref fs))))

(defn- any-futures-failed? [fs]
  (some false? (map deref (filter future-done? fs))))

(defn- terminate-and-return [fs atom return]
  (terminate atom)
  (doall (map deref fs))
  return)

(defmethod run :parallel-sequence [{children :children} & [terminate?]]
  (if (run-action? terminate?)
    (let [parent-terminate? terminate?
          self-terminate? (atom false)
          fs (map #(future (run % self-terminate?)) children)]
      (loop []
        (Thread/sleep 50)
        (cond (not (run-action? parent-terminate?)) (terminate-and-return fs self-terminate? false)
              (all-futures-succeded? fs) true
              (any-futures-failed? fs)  (terminate-and-return fs self-terminate? false)
              :default (recur))))
    false))
