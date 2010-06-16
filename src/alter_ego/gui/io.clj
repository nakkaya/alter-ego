(ns alter-ego.gui.io
  (:import (javax.swing.tree DefaultTreeModel DefaultMutableTreeNode)))


(defn save-tree 
  ([model file]
     (binding [*out* (java.io.FileWriter. file)]
       (prn (save-tree (.getRoot model)))))
  ([node]
     (let [children (.children node)
	   node (.getUserObject node)] 
       (reduce (fn[h v] (conj h (save-tree v))) [node] 
	       (enumeration-seq children)))))

(defn load-tree 
  ([file]
     (let [tree (read-string (slurp (.getPath file)))
	   root (with-meta (first tree) {:file file})] 
       (load-tree (DefaultMutableTreeNode. root) (rest tree))))
  ([parent children]
     (doall 
      (map #(if (> (count %) 1)
	      (let [p (DefaultMutableTreeNode. (first %))
		    c (rest %)]
		(.add parent p)
		(load-tree p c))
	      (.add parent (DefaultMutableTreeNode. (first %)))) children))
     parent))
