(ns alter-ego.gui.tree-actions
  (:use [alter-ego.gui.util :only [add-action-listener image-icon]])
  (:use [alter-ego.gui.io :only [load-tree save-tree]])
  (:use [alter-ego.gui.edit-node :only [edit-node]])
  (:import (javax.swing SwingUtilities JPanel JButton JFileChooser)
	   (javax.swing.tree DefaultMutableTreeNode)))

(defn tree-node [type name]
  (DefaultMutableTreeNode. {:type type :name name}))

(defn new-action [event tree]
  (-> tree .getModel 
      (.setRoot 
       (DefaultMutableTreeNode. {:type :selector :name "Root"}))))

(defn open-action [event tree]
  (let [fc (JFileChooser.)] 
    (.setFileSelectionMode fc JFileChooser/FILES_ONLY)
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showOpenDialog fc (.getSource event)))
      (-> tree .getModel 
	  (.setRoot (load-tree (.getSelectedFile fc)))))))

(defn save-action [event tree]
  (let [fc (JFileChooser.)] 
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showSaveDialog fc (.getSource event)))
      (save-tree (-> tree .getModel) (.getSelectedFile fc)))))

(defn move-down-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	parent (.getParent chosen)
	model (.getModel tree)
	i (.getIndexOfChild model parent chosen)
	child-cnt (.getChildCount parent)
	selected-index (.getMinSelectionRow tree)]
    (if (< i (dec child-cnt))
      (do (.removeNodeFromParent model chosen)
	  (.insertNodeInto model chosen parent (inc i))
	  (.setSelectionRow tree (inc selected-index))))))

(defn move-up-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	parent (.getParent chosen)
	model (.getModel tree)
	i (.getIndexOfChild model parent chosen)
	selected-index (.getMinSelectionRow tree)]
    (if (pos? i)
      (do (.removeNodeFromParent model chosen)
	  (.insertNodeInto model chosen parent (dec i))
	  (.setSelectionRow tree (dec selected-index))))))

(defn edit-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (doto (edit-node tree chosen)
	(.setLocationRelativeTo tree)
	(.setVisible true)))))


(defn insert-action [event tree type]
  (let [chosen (.getLastSelectedPathComponent tree)
	child-cnt (.getChildCount chosen)
	child (tree-node type "new")]
    (if-not (nil? chosen)
      (-> tree .getModel (.insertNodeInto child chosen child-cnt)))))

(defn remove-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (-> tree .getModel (.removeNodeFromParent chosen)))))
