(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.tree-actions
  (:use [alter-ego.gui.util :only [add-action-listener image-icon]])
  (:use [alter-ego.gui.node-types] :reload-all)
  (:use [alter-ego.gui.io :only [load-tree save-tree]])
  (:use [alter-ego.gui.export :only [export-tree]])
  (:import (javax.swing SwingUtilities JPanel JButton JFileChooser)
	   (javax.swing.tree DefaultMutableTreeNode TreePath)))

(defn tree-node [type name]
  (DefaultMutableTreeNode. (merge {:type type :name name} 
				  (:opts (node-types type)))))

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

(defn export-action [event tree]
  (let [model (-> tree .getModel)
	meta (meta (-> model .getRoot .getUserObject))
	fc (JFileChooser.)]
    (.setDialogTitle fc "Export to Graphviz")

    (if-not (nil? meta)
      (let [file (:file meta)
	    name (str (re-find #".*\." (.getName file)) "gv")] 
	(.setSelectedFile fc (java.io.File. file name))))

    (if (= JFileChooser/APPROVE_OPTION 
	   (.showSaveDialog fc (.getSource event)))
      (let [file (.getSelectedFile fc)
	    model (-> tree .getModel)]
	(export-tree model file)))))

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
  (let [chosen (.getSelectionPath tree)]
    (.startEditingAtPath tree chosen)
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

(defn paste-action [event tree ccp]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (and (nil? chosen)
		 (not (empty? @ccp)))
      (let [nodes (first @ccp)] 
	(doseq [[_ _ node] nodes] 
	  (-> tree .getModel (.insertNodeInto node chosen 0))
	  (dosync (ref-set ccp (rest @ccp))))
	(tree-modified tree)))))

(defn cut-action [event tree ccp]
  (let [selections (selections tree (.getSelectionPaths tree))] 
    (if-not (empty? selections)
      (do 
	(doseq [[index parent chosen] selections]
	  (-> tree .getModel (.removeNodeFromParent chosen)))
	(dosync (alter ccp conj selections))
	(tree-modified tree)))))

(defn copy-action [event tree ccp]
  (let [selections (selections tree (.getSelectionPaths tree))] 
    (if-not (empty? selections)
      (dosync (alter ccp conj 
		     (map #(let [[_ _ chosen] %] 
			     [-1 -1 (DefaultMutableTreeNode. 
				      (.getUserObject chosen))])
			  selections))))))

(defn disable-node-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (let [obj (.getUserObject chosen)] 
	(.setUserObject chosen (assoc obj :status :disabled))
	(tree-modified tree)))))

(defn enable-node-action [event tree]
  (let [chosen (.getLastSelectedPathComponent tree)]
    (if-not (nil? chosen)
      (let [obj (.getUserObject chosen)] 
	(.setUserObject chosen (dissoc obj :status))
	(tree-modified tree)))))
