(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.node-types)

(def node-types
     {:action {:name "Action" 
	       :icon "action.png"
	       :type :action
	       :opts {:function "nil"}}
      :selector {:name "Selector" 
		 :icon "selector.png"
		 :type :composite}
      :non-deterministic-selector {:name "Non Deterministic Selector" 
				   :icon "selector.png"
				   :type :composite}
      :sequence {:name "Sequence" 
		 :icon "sequence.png"
		 :type :composite}
      :non-deterministic-sequence {:name "Non Deterministic Sequence" 
				   :icon "sequence.png"
				   :type :composite}
      :until-fail {:name "Until Fail" 
		   :icon "until.png"
		   :type :decorator}
      :until-success {:name "Until Success" 
		      :icon "until.png"
		      :type :decorator}
      :limit {:name "Limit" 
	      :icon "limit.png"
	      :type :decorator
	      :opts {:times 1}}
      :inverter {:name "Inverter" 
		 :icon "inverter.png"
		 :type :decorator}
      :try-catch {:name "Try Catch" 
		  :icon "try-catch.png"
		  :type :decorator}
      :print-blackboard {:name "Print Blackboard"
			 :icon "debug.png"
			 :type :decorator}})
