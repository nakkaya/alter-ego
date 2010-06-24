(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.gui.export
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(defn node-name [idx]
  (str "N_" idx))

(defn node-desc [idx node]
  (let [{node :name type :type} (.getUserObject node)] 
    (if (= type :action) 
      (str (node-name idx) 
	   " [shape=\"box\" style=filled fillcolor=\"#00ff005f\" label=\"" 
	   node "\"];\n")
      (str (node-name idx) 
	   " [label=\"" node "\\n(" (name type) ")\"];\n"))))

(defn export-tree 
  ([model file]
     (doto (java.io.FileWriter. file)
       (.write (.toString (export-tree (.getRoot model))))
       (.close)))
  ([root]
     (let [idx-counter (let [c (atom 0)] #(swap! c inc))
	   content (StringBuffer.)] 
       (.append content "digraph bt {\n")
       (.append content (node-desc 0 root))
       (export-tree root 0 idx-counter content)
       (.append content "}")
       content))
  ([node parent-idx idx-counter content]
     (doseq [child (enumeration-seq (.children node))]
       (let [idx (idx-counter)] 
	 (.append content (node-desc idx child))
	 (.append content (str (node-name parent-idx) " -> " 
			       (node-name idx) "\n"))
	 (export-tree child idx idx-counter content)))))
