(ns alter-ego.sample-actions
  (:refer-clojure :exclude [sequence])
  (:use [alter-ego.core] :reload-all))

(defn inc-i [blackboard]
  (let [{i :i} @blackboard] 
    (dosync (alter blackboard assoc :i (inc i)))))

(defn dec-i [blackboard]
  (let [{i :i} @blackboard] 
    (dosync (alter blackboard assoc :i (dec i)))))

(defn get-i [blackboard]
  (:i @blackboard))

(defn inc-i-action [blackboard]
  (action 'alter-ego.sample-actions/inc-i blackboard))

(defn dec-i-action [blackboard]
  (action 'alter-ego.sample-actions/dec-i blackboard))

(defn door-open? [blackboard]
  (:door-open @blackboard))

(defn door-open?-action [blackboard]
  (action 'alter-ego.sample-actions/door-open? blackboard))

(defn small? [blackboard]
  (if (< (:i @blackboard) 5) true false))

(defn small?-action [blackboard]
  (action 'alter-ego.sample-actions/small? blackboard))

(defn to-room [blackboard]
  (dosync (alter blackboard assoc :to-room true)))

(defn to-room-action [blackboard]
  (action 'alter-ego.sample-actions/to-room blackboard))

(defn to-door [blackboard]
  (dosync (alter blackboard assoc :to-door true)))

(defn to-door-action [blackboard]
  (action 'alter-ego.sample-actions/to-door blackboard))

(defn open-door [blackboard]
  (dosync (alter blackboard assoc :open-door true)))

(defn open-door-action [blackboard]
  (action 'alter-ego.sample-actions/open-door blackboard))

(defn throw-exception [blackboard]
  (throw (Exception. "Some Error")) true)

(defn throw-exception-action [blackboard]
  (action 'alter-ego.sample-actions/throw-exception blackboard))
