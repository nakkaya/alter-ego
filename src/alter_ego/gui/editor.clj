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
  (let [popup (JPopupMenu.)
	insert-menu (JMenu. "Insert")
	selector (JMenuItem. "Selector")
	sequence (JMenuItem. "Sequence")
	action (JMenuItem. "Action")
	edit (JMenuItem. "Edit")
	remove (JMenuItem. "Remove")]

    (add-action-listener selector insert-action tree :selector)
    (add-action-listener sequence insert-action tree :sequence)
    (add-action-listener action insert-action tree :action)
    (add-action-listener remove remove-action tree)
    (add-action-listener edit edit-action tree)

    (doto insert-menu
      (.add selector)
      (.add sequence)
      (.add action))

    (doto popup
      (.add insert-menu)
      (.add edit)
      (.addSeparator)
      (.add remove))))

(defn mouse-adapter [tree]
  (let [popup (popup tree)
	show #(.show %1 (.getComponent %2) (.getX %2) (.getY %2))] 
    (proxy [MouseAdapter] []
      (mousePressed [e] (if (.isPopupTrigger e) (show popup e)))
      (mouseReleased [e] (if (.isPopupTrigger e) (show popup e))))))

(defn cell-icon [type]
  (cond (= type :action) (image-icon "action.png")
	(= type :selector) (image-icon "selector.png")
	(= type :sequence) (image-icon "sequence.png")
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
