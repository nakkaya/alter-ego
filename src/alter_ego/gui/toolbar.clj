(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.toolbar
  (:use [alter-ego.gui.util :only [add-action-listener image-icon]])
  (:use [alter-ego.gui.tree-actions])
  (:import (javax.swing SwingUtilities JPanel JButton)
	   (net.miginfocom.swing MigLayout)))

(defn button 
  ([i s f tree]
     (doto (JButton.)
       (.setToolTipText s)
       (.putClientProperty "JButton.buttonType" "gradient")
       (.setIcon (image-icon i))
       (add-action-listener f tree)))
  ([i s f tree ccp]
     (doto (JButton.)
       (.setToolTipText s)
       (.putClientProperty "JButton.buttonType" "gradient")
       (.setIcon (image-icon i))
       (add-action-listener f tree ccp))))

(defn toolbar [tree ccp]
  (let [toolbar (JPanel. (MigLayout. "insets 0 0 0 0"))]
    (doto toolbar
      (.add (button "new.png" "New" new-action tree))
      (.add (button "open.png" "Open" open-action tree))
      (.add (button "save.png" "Save" save-action tree))
      (.add (button "save-as.png" "Save As" 
		    save-as-action tree))
      (.add (button "export.png" "Export to Graphviz" 
		    export-action tree) "gapright 20")
      (.add (button "copy.png" "Copy Node" copy-action tree ccp))
      (.add (button "cut.png" "Cut Node" cut-action tree ccp))
      (.add (button "paste.png" "Paste Node" 
		    paste-action tree ccp) "gapright 20")
      (.add (button "expand-tree.png" "Expand Tree" 
		    expand-tree-action tree) "gapright 20")
      (.add (button "up-arrow.png" "Move Node Up" move-up-action tree))
      (.add (button "down-arrow.png" "Move Node Down" 
		    move-down-action tree)))))

(comment 
  (defn frame []
    (let [toolbar (toolbar nil)] 
      (doto (javax.swing.JFrame. "Tree Editor")
	(.add toolbar)
	(.pack)
	(.setLocationRelativeTo nil)
	(.setVisible true))))

  (SwingUtilities/invokeLater frame)
  )
