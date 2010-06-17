(ns alter-ego.gui.tree-actions
  (:use [alter-ego.gui.util :only [add-action-listener image-icon]])
  (:use [alter-ego.gui.io :only [load-tree save-tree]])
  (:use [alter-ego.gui.edit-node :only [edit-node]])
  (:import (javax.swing SwingUtilities JPanel JButton JFileChooser)
	   (javax.swing.tree DefaultMutableTreeNode TreePath)))

(defn tree-node [type name]
  (DefaultMutableTreeNode. {:type type :name name}))

(defn- frame [node source]
  ((resolve 'alter-ego.gui.editor/frame) node source))

(defn- tree-modified [tree]
  (let [frame (SwingUtilities/getRoot tree)
	title (.getTitle frame)]
    (if-not (= (take 2 title) '(\* \*))
      (.setTitle frame (str "** " title)))))

(defn- tree-saved [tree]
  (let [frame (SwingUtilities/getRoot tree)
	title (.getTitle frame)]
    (if (= (take 2 title) '(\* \*))
      (.setTitle frame (apply str (drop 3 title))))))

(defn new-action [event tree]
  (frame (tree-node :selector "Root") (.getSource event)))

(defn open-action [event tree]
  (let [fc (JFileChooser.)] 
    (.setFileSelectionMode fc JFileChooser/FILES_ONLY)
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showOpenDialog fc (.getSource event)))
      (let [file (.getSelectedFile fc)]
	(frame (load-tree file) (.getSource event))))))

(defn save-as-action [event tree]
  (let [fc (JFileChooser.)] 
    (if (= JFileChooser/APPROVE_OPTION 
	   (.showSaveDialog fc (.getSource event)))
      (let [file (.getSelectedFile fc)
	    frame (SwingUtilities/getRoot tree)
	    model (-> tree .getModel)
	    obj (-> model .getRoot .getUserObject)
	    with-meta (with-meta obj {:file file})]
	(save-tree model file)
	(-> model .getRoot (.setUserObject with-meta))
	(.setTitle frame (.getName file))))))

(defn save-action [event tree]
  (let [model (-> tree .getModel)
	meta (meta (-> model .getRoot .getUserObject))]
    (if (nil? meta)
      (save-as-action event tree)
      (do 
	(save-tree model (:file meta))
	(tree-saved tree)))))

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
    	(.setSelectionRows tree (int-array (map inc rows)))
	(tree-modified tree)))))

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
	(.setSelectionRows tree (int-array (map dec rows)))
	(tree-modified tree)))))

(defn edit-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (doto (edit-node tree chosen)
	(.setLocationRelativeTo tree)
	(.setVisible true)))
    (tree-modified tree)))


(defn insert-action [event tree type]
  (let [chosen (.getLastSelectedPathComponent tree)
	child-cnt (.getChildCount chosen)
	child (tree-node type "new")]
    (if-not (nil? chosen)
      (-> tree .getModel (.insertNodeInto child chosen child-cnt)))
    (tree-modified tree)))

(defn remove-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (-> tree .getModel (.removeNodeFromParent chosen)))
    (tree-modified tree)))

(defn expand-tree-action [event tree]
  (loop [row 0 total 1]
    (if-not (> row total)
      (do 
	(.expandRow tree row)
	(recur (inc row) (.getRowCount tree))))))
