(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.io
  (:import (javax.swing.tree DefaultTreeModel DefaultMutableTreeNode)))


(defn save-tree 
  ([model file]
     (binding [*out* (java.io.FileWriter. file)]
       (prn (save-tree (.getRoot model)))))
  ([node]
     (let [children (enumeration-seq (.children node))
           parent (.getUserObject node)]

       (if (empty? children)
         parent
         (assoc parent :children (reduce
                                  (fn[h v]
                                    (conj h (save-tree v)))
                                  [] children))))))

(defmulti load-tree class)

(defmethod load-tree java.io.File [file]
  (let [tree (read-string (slurp (.getPath file)))] 
    (load-tree tree)))

(defmethod load-tree clojure.lang.PersistentArrayMap [node]
  (if (nil? (node :children))
    (DefaultMutableTreeNode. node)
    (let [children (:children node)]
      (reduce (fn[h v]
                (doto h
                  (.add (load-tree v))))
              (DefaultMutableTreeNode. (dissoc node :children)) (:children node)))))