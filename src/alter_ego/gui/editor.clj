(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.editor
  (:use [alter-ego.gui.util :only [add-action-listener 
				   add-key-typed-listener image-icon]])
  (:use [alter-ego.gui.toolbar :only [toolbar]] :reload-all)
  (:use [alter-ego.gui.tree-actions] :reload-all)
  (:import (javax.swing SwingUtilities JScrollPane JTree JPanel)
	   (javax.swing.tree DefaultTreeCellRenderer DefaultMutableTreeNode)
	   (java.awt.event MouseAdapter)
	   (javax.swing JPopupMenu JMenu JMenuItem)
	   (javax.swing.border LineBorder)
	   (net.miginfocom.swing MigLayout))
  (:gen-class))

(defn popup [tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	{status :status} (.getUserObject chosen)
	item (fn 
	       ([s f tree type]
		  (doto (JMenuItem. s)
		    (add-action-listener f tree type)))
	       ([s f tree]
		    (doto (JMenuItem. s)
		      (add-action-listener f tree))))
	popup (JPopupMenu.)
	insert-menu (JMenu. "Insert")]

    (doto insert-menu
      (.add (item "Action" insert-action tree :action))
      (.addSeparator)
      (.add (item "Selector" insert-action tree :selector))
      (.add (item "Non Deterministic Selector" 
		  insert-action tree :non-deterministic-selector))
      (.add (item "Seqence" insert-action tree :sequence))
      (.add (item "Non Deterministic Sequence" 
		  insert-action tree :non-deterministic-sequence))
      (.addSeparator)
      (.add (item "Until Fail" insert-action tree :until-fail))
      (.add (item "Until Success" insert-action tree :until-success))
      (.add (item "Limit" insert-action tree :limit))
      (.add (item "Inverter" insert-action tree :inverter)))

    (doto popup
      (.add insert-menu)
      (.add (item "Edit" edit-action tree))
      (.addSeparator)
      (.add (if (or (nil? status)
      		    (= :disable status))
      	      (item "Disable" disable-node-action tree)
      	      (item "Enable" enable-node-action tree)))
      (.add (item "Remove" remove-action tree)))))

(defn mouse-adapter [tree]
  (let [show #(.show (popup tree) (.getComponent %1) (.getX %1) (.getY %1))] 
    (proxy [MouseAdapter] []
      (mousePressed [e] (if (.isPopupTrigger e) (show e)))
      (mouseReleased [e] (if (.isPopupTrigger e) (show e))))))

(defn cell-icon [type status]
  (cond (and (not (nil? status))
	     (= status :disabled)) (image-icon "disabled.png")
	(= type :action) (image-icon "action.png")
	(= type :selector) (image-icon "selector.png")
	(= type :non-deterministic-selector) (image-icon "selector.png")
	(= type :sequence) (image-icon "sequence.png")
	(= type :non-deterministic-sequence) (image-icon "sequence.png")
	(= type :until-fail) (image-icon "until.png")
	(= type :until-success) (image-icon "until.png")
	(= type :inverter) (image-icon "inverter.png")
	(= type :limit) (image-icon "limit.png")
	:else (image-icon "action.png")))

(defn cell-renderer []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent
     [tree value selected expanded leaf row has-focus?]
     (let [{type :type n :name status :status} (.getUserObject value)]
       (if (and (not (nil? status))
		(= status :disabled))
	 (.setText this (str n " (" (name status) "/" (name type) ")"))
	 (.setText this (str n " (" (name type) ")")))
       (.setOpaque this true)
       (.setIcon this (cell-icon type status))
       (.setIconTextGap this 10)
       (.setBackground this java.awt.Color/white)
       (if selected
	 (.setBorder this (LineBorder. (java.awt.Color. 147 157 195) 3 true))
	 (.setBorder this nil))
       this))))

(defn tree [node]
  (let [tree (JTree. node)]
    (.setSelectionMode 
     (.getSelectionModel tree) 
     javax.swing.tree.TreeSelectionModel/CONTIGUOUS_TREE_SELECTION)
    (doto tree
      (.setRowHeight 30)
      (.setShowsRootHandles true)
      (.setCellRenderer (cell-renderer))
      (.addMouseListener (mouse-adapter tree))
      (.setSelectionRow 0))))

(defn key-bindings [event tree ccp]
  (let [modifier (.getModifiers event)
	key (.getKeyChar event)] 
    (when (= 4 modifier) ;;alt pressed
      (cond (= key \n) (new-action event tree)
	    (= key \o) (open-action event tree)
	    (= key \s) (save-action event tree)
	    (= key \e) (edit-action event tree)
	    (= key \v) (paste-action event tree ccp)
	    (= key \x) (cut-action event tree ccp)
	    (= key \c) (copy-action event tree ccp)))))

(defn frame [node & args]
  (let [[parent-component] args
	panel (JPanel. (MigLayout. "fill,insets 0 0 0 0"))
	ccp (ref [])
	tree (tree node)
	toolbar (toolbar tree ccp)
	meta (meta (.getUserObject node))
	title (if (nil? meta) "scratch" (.getName (:file meta)))]
    (add-key-typed-listener tree key-bindings tree ccp)
    (doto panel
      (.add toolbar "wrap")
      (.add (JScrollPane. tree) "grow"))
    (doto (javax.swing.JFrame. title)
      (.add panel)
      (.pack)
      (.setLocationRelativeTo parent-component)
      (.setVisible true))))

(defn -main [& args]
  (let [frame #(frame (tree-node :selector "Root"))] 
    (SwingUtilities/invokeLater frame)))
