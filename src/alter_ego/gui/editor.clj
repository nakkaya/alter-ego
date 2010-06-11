(ns alter-ego.gui.editor
  (:use [clojure.contrib.swing-utils :only [add-action-listener]])
  (:use [alter-ego.gui.edit-node :only [edit-node]])
  (:use [alter-ego.gui.io :only [load-tree save-tree]])
  (:import (javax.swing JScrollPane JTree)
	   (javax.swing.tree DefaultTreeCellRenderer DefaultMutableTreeNode)
	   (java.awt.event MouseAdapter)
	   (javax.swing JPopupMenu JMenu JMenuItem JFileChooser)))

(defn tree-node [type name]
  (DefaultMutableTreeNode. {:type type :name name}))

(defn tree-insert [event tree type]
  (let [chosen (.getLastSelectedPathComponent tree)
	child-cnt (.getChildCount chosen)
	child (tree-node type "new")]
    (if-not (nil? chosen)
      (-> tree .getModel (.insertNodeInto child chosen child-cnt)))))

(defn tree-remove [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (-> tree .getModel (.removeNodeFromParent chosen)))))

(defn tree-edit [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (doto (edit-node tree chosen)
	(.setLocationRelativeTo tree)
	(.setVisible true)))))

(defn tree-move-up [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	parent (.getParent chosen)
	model (.getModel tree)
	i (.getIndexOfChild model parent chosen)]
    (if (pos? i)
      (do (.removeNodeFromParent model chosen)
	  (.insertNodeInto model chosen parent (dec i))))))

(defn tree-move-down [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	parent (.getParent chosen)
	model (.getModel tree)
	i (.getIndexOfChild model parent chosen)
	child-cnt (.getChildCount parent)]
    (if (< i (dec child-cnt))
      (do (.removeNodeFromParent model chosen)
	  (.insertNodeInto model chosen parent (inc i))))))

(defn tree-new-tree [event tree]
  (-> tree .getModel (.setRoot (tree-node :selector "Root"))))

(defn tree-save-tree [event tree]
  (let [fc (JFileChooser.)] 
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showSaveDialog fc (.getSource event)))
      (save-tree (-> tree .getModel) (.getSelectedFile fc)))))

(defn tree-load-tree [event tree]
  (let [fc (JFileChooser.)] 
    (.setFileSelectionMode fc JFileChooser/FILES_ONLY)
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showOpenDialog fc (.getSource event)))
      (-> tree .getModel 
	  (.setRoot (load-tree (.getSelectedFile fc)))))))

(defn popup [tree]
  (let [popup (JPopupMenu.)
	insert-menu (JMenu. "Insert")
	selector (JMenuItem. "Selector")
	sequence (JMenuItem. "Sequence")
	action (JMenuItem. "Action")
	edit (JMenuItem. "Edit")
	move-up (JMenuItem. "Move Up")
	move-down (JMenuItem. "Move down")
	remove (JMenuItem. "Remove")
	file-menu (JMenu. "File")
	new (JMenuItem. "New")
	open (JMenuItem. "Open")
	save (JMenuItem. "Save")]

    (add-action-listener selector tree-insert tree :selector)
    (add-action-listener sequence tree-insert tree :sequence)
    (add-action-listener action tree-insert tree :action)
    (add-action-listener remove tree-remove tree)
    (add-action-listener edit tree-edit tree)
    (add-action-listener move-up tree-move-up tree)
    (add-action-listener move-down tree-move-down tree)
    (add-action-listener new tree-new-tree tree)
    (add-action-listener save tree-save-tree tree)
    (add-action-listener open tree-load-tree tree)

    (doto file-menu
      (.add new)
      (.add open)
      (.add save))

    (doto insert-menu
      (.add selector)
      (.add sequence)
      (.add action))

    (doto popup
      (.add insert-menu)
      (.add edit)
      (.addSeparator)
      (.add move-up)
      (.add move-down)
      (.addSeparator)
      (.add remove)
      (.add file-menu))))

(defn mouse-adapter [tree]
  (let [popup (popup tree)
	show #(.show %1 (.getComponent %2) (.getX %2) (.getY %2))] 
    (proxy [MouseAdapter] []
      (mousePressed [e] (if (.isPopupTrigger e) (show popup e)))
      (mouseReleased [e] (if (.isPopupTrigger e) (show popup e))))))

(defn cell-renderer []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent
     [tree value selected expanded leaf row has-focus?]
     (let [{type :type n :name} (.getUserObject value)]
       (.setText this (str n " (" (name type) ")"))
       (.setOpaque this true)
       (if selected
	 (do (.setBackground this (java.awt.Color. 147 157 195))
	     (.setForeground this (java.awt.Color. 255 255 247)))
	 (do (.setBackground this java.awt.Color/white)
	     (.setForeground this (java.awt.Color/black))))
       this))))

(defn tree []
  (let [tree (JTree. (tree-node :selector "Root"))]
    (doto tree
      (.setShowsRootHandles true)
      (.setCellRenderer (cell-renderer))
      (.addMouseListener (mouse-adapter tree)))
    (JScrollPane. tree)))

(defn editor []
  (let [tree (tree)] 
    (doto (javax.swing.JFrame. "Tree Editor")
      (.add tree)
      (.pack)
      (.setSize 200 400)
      (.setLocationRelativeTo nil)
      (.setVisible true))))
