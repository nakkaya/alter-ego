(ns alter-ego.gui.editor
  (:use [alter-ego.gui.util :only [add-action-listener image-icon]])
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
  (let [item (fn 
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
      (.add (item "Limit" insert-action tree :limit))
      (.add (item "Inverter" insert-action tree :inverter)))

    (doto popup
      (.add insert-menu)
      (.add (item "Edit" edit-action tree))
      (.addSeparator)
      (.add (item "Remove" remove-action tree)))))

(defn mouse-adapter [tree]
  (let [popup (popup tree)
	show #(.show %1 (.getComponent %2) (.getX %2) (.getY %2))] 
    (proxy [MouseAdapter] []
      (mousePressed [e] (if (.isPopupTrigger e) (show popup e)))
      (mouseReleased [e] (if (.isPopupTrigger e) (show popup e))))))

(defn cell-icon [type]
  (cond (= type :action) (image-icon "action.png")
	(= type :selector) (image-icon "selector.png")
	(= type :non-deterministic-selector) (image-icon "selector.png")
	(= type :sequence) (image-icon "sequence.png")
	(= type :non-deterministic-sequence) (image-icon "sequence.png")
	(= type :until-fail) (image-icon "until-fail.png")
	(= type :inverter) (image-icon "inverter.png")
	(= type :limit) (image-icon "limit.png")
	:else (image-icon "action.png")))

(defn cell-renderer []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent
     [tree value selected expanded leaf row has-focus?]
     (let [{type :type n :name} (.getUserObject value)]
       (.setText this (str n " (" (name type) ")"))
       (.setOpaque this true)
       (.setIcon this (cell-icon type))
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
      (.addMouseListener (mouse-adapter tree)))))

(defn frame [node & args]
  (let [[parent-component] args
	panel (JPanel. (MigLayout. "fill,insets 0 0 0 0"))
	tree (tree node)
	toolbar (toolbar tree)
	meta (meta (.getUserObject node))
	title (if (nil? meta) "scratch" (.getName (:file meta)))]
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
