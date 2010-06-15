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

(defn- selections [tree selections]
  (let [model (.getModel tree)] 
    (sort-by 
     first
     (reduce (fn[h v] 
	       (let [chosen (.getLastPathComponent v)
		     parent (.getLastPathComponent (.getParentPath v))
		     index (.getIndexOfChild model parent chosen)]
		 (conj h [index parent chosen])))
	     [] selections))))

(defn move-down-action [event tree]
  (let [model (.getModel tree)
	selections (selections tree (.getSelectionPaths tree))
	rows (.getSelectionRows tree)
	parent (second (first selections))
	valid? (< (inc (first (last selections))) (.getChildCount parent))]

    (if valid?
      (do 
    	(doseq [[index parent chosen] (reverse selections)] 
    	  (.removeNodeFromParent model chosen)
    	  (.insertNodeInto model chosen parent (inc index)))
    	(.setSelectionRows tree (int-array (map inc rows)))))))

(defn move-up-action [event tree]
  (let [model (.getModel tree)
	selections (selections tree (.getSelectionPaths tree))
	rows (.getSelectionRows tree)
	valid? (> (first (first selections)) 0)]
    (if valid?
      (do 
	(doseq [[index parent chosen] selections] 
	  (.removeNodeFromParent model chosen)
	  (.insertNodeInto model chosen parent (dec index)))
	(.setSelectionRows tree (int-array (map dec rows)))))))

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
