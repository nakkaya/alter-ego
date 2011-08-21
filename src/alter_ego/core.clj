(ns #^{:author "Nurullah Akkaya"}
  alter-ego.core
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all))

(defn- node [n blackboard]
  (let [{t :type func :function string :string} n]
    (cond (= t :action) (if (nil? (resolve (symbol func)))
			  (throw (Exception. "Symbol not defined."))
			  (action (symbol func) blackboard))
	  (= t :selector) (selector)
	  (= t :non-deterministic-selector) (non-deterministic-selector)
	  (= t :sequence) (sequence)
	  (= t :non-deterministic-sequence) (non-deterministic-sequence)
	  (= t :until-fail) (until-fail nil)
	  (= t :until-success) (until-success nil)
	  (= t :limit) (limit nil)
	  (= t :inverter) (inverter nil)
	  (= t :print-blackboard) (print-blackboard blackboard nil)
	  (= t :print-string) (print-string string nil)
	  (= t :break-point) (break-point nil)
	  :default (throw (Exception. "Unknown node type.")))))

(defn- append-child [p c]
  (assoc p :children (reverse (conj (reverse (:children p)) c))))

(defn load-tree 
  "Load tree definition."
  ([file]
     (load-tree file (ref {})))
  ([file blackboard]
     (let [tree (read-string (slurp file))] 
       (load-tree (node (first tree) blackboard) (rest tree) blackboard)))
  ([parent children blackboard]
     (reduce (fn[h v]
	       (if (> (count v) 1)
	         (let [p (node (first v) blackboard)
	               c (rest v)]
	           (if (nil? (:status (first v)))
	             (append-child h (load-tree p c blackboard))
	             h))
	         (if (nil? (:status (first v)))
	           (append-child h (node (first v) blackboard))
	           h)))
	     parent children)))

(defn exec [t]
  (run t))

(defmacro from-blackboard 
  "A convenience macro to lookup bindings in the given blackboard."
  [blackboard bindings & body]
  `(let [~@(interleave bindings
		       (map cons 
			    (map keyword bindings)
			    (repeat (list (list 'deref `~blackboard)))))]
     ~@body))

(defmacro defaction
  "Define an action which looks up its bindings from blackboard."
  [name params & body]
  `(defn ~name [~'blackboard] 
     (from-blackboard ~'blackboard ~params
       ~@body)))
