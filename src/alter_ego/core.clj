(ns alter-ego.core
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.node-types] :reload-all)
  (:use [alter-ego.composite] :reload-all)
  (:use [alter-ego.decorator] :reload-all))

(defn- node [n blackboard]
  (let [{t :type func :action} n]
    (cond (= t :action) (action (symbol func) blackboard)
	  (= t :selector) (selector [])
	  (= t :non-deterministic-selector) (non-deterministic-selector [])
	  (= t :sequence) (sequence [])
	  (= t :non-deterministic-sequence) (non-deterministic-sequence [])
	  (= t :until-fail) (until-fail nil)
	  (= t :limit) (limit nil)
	  (= t :inverter) (inverter nil))))

(defmethod append-child [:alter-ego.node-types/composite 
			 :alter-ego.node-types/type] [p c] 
  (assoc p :children (conj (:children p) c)))

(defmethod append-child [:alter-ego.node-types/decorator
			 :alter-ego.node-types/type] [p c] 
  (assoc p :children c))

(defn load-tree 
  ([file]
     (load-tree file (ref {})))
  ([file blackboard]
     (let [tree (read-string (slurp file))] 
       (load-tree (first tree) (rest tree) blackboard)))
  ([parent children blackboard]
     (reduce (fn[h v] 
	       (if (> (count v) 1)
		 (let [p (node (first v) blackboard)
		       c (rest v)] 
		   (append-child h (load-tree p c blackboard)))
		 (append-child h (node (first v) blackboard)))) 
	     parent children)))
