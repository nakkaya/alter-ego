(ns #^{:author "Nurullah Akkaya"
       :skip-wiki true}
  alter-ego.editor
  (:import (javax.swing SwingUtilities JScrollPane JTree JPanel)
	   (javax.swing.tree DefaultTreeCellRenderer DefaultMutableTreeNode 
			     DefaultTreeCellEditor DefaultTreeModel TreePath)
	   (java.awt.event MouseAdapter)
	   (javax.swing ImageIcon JToolBar JButton JFrame
                        JPopupMenu JMenu JMenuItem JFileChooser)
	   (javax.swing.border LineBorder)
	   (java.awt BorderLayout))
  (:gen-class))

;;
;; Misc
;;

(defn add-action-listener
  [component f & args]
  (let [listener (proxy [java.awt.event.ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn add-key-typed-listener
  [component f & args]
  (let [listener (proxy [java.awt.event.KeyAdapter] []
                   (keyTyped [event] (apply f event args)))]
    (.addKeyListener component listener)
    listener))

(defn image-icon [file]
  (if-let[res (ClassLoader/getSystemResource file)] 
    (ImageIcon. res)
    (ImageIcon. (ClassLoader/getSystemResource (str "resources/" file)))))

;;
;; graphviz
;;

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

;;
;; I/O
;;

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

;;
;; nodes
;;

(def node-types
     {:action {:name "Action" 
	       :icon "action.png"
	       :type :action
	       :opts {:function "nil"}}
      :selector {:name "Selector" 
		 :icon "selector.png"
		 :type :composite}
      :non-deterministic-selector {:name "Non Deterministic Selector" 
				   :icon "selector.png"
				   :type :composite}
      :sequence {:name "Sequence" 
		 :icon "sequence.png"
		 :type :composite}
      :non-deterministic-sequence {:name "Non Deterministic Sequence" 
				   :icon "sequence.png"
				   :type :composite}
      :until-fail {:name "Until Fail" 
		   :icon "until.png"
		   :type :decorator}
      :until-success {:name "Until Success" 
		      :icon "until.png"
		      :type :decorator}
      :limit {:name "Limit" 
	      :icon "limit.png"
	      :type :decorator
	      :opts {:times 1}}
      :inverter {:name "Inverter" 
		 :icon "inverter.png"
		 :type :decorator}
      :try-catch {:name "Try Catch" 
		  :icon "try-catch.png"
		  :type :decorator}
      :print-blackboard {:name "Print Blackboard"
			 :icon "debug.png"
			 :type :decorator}
      :print-string {:name "Print String"
		     :icon "debug.png"
		     :type :decorator
		     :opts {:string "nil"}}
      :break-point {:name "Breakpoint"
		    :icon "debug.png"
		    :type :decorator}})

;;
;; actions
;;

(declare frame)

(defn tree-node [type name]
  (DefaultMutableTreeNode. (merge {:type type :name name} 
				  (:opts (node-types type)))))

(defn tree-modified [tree]
  (let [frame (SwingUtilities/getRoot tree)
	title (.getTitle frame)]
    (if-not (= (take 2 title) '(\* \*))
      (.setTitle frame (str "** " title)))))

(defn tree-saved [tree]
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

(defn selections [tree selections]
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

(defn insert-into-tree [tree obj]
  (let [chosen (.getLastSelectedPathComponent tree)
  	child-cnt (.getChildCount chosen)]
    (if-not (nil? chosen)
      (-> tree .getModel (.insertNodeInto obj chosen child-cnt)))
    (tree-modified tree)))

(defn insert-action [event tree type]
  (insert-into-tree tree (tree-node type "new")))

(defn insert-defined-action [event tree obj]
  (insert-into-tree tree (DefaultMutableTreeNode. obj)))

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

(defn decorate-with-action [event tree type]
  (let [model (.getModel tree)
	chosen (.getLastSelectedPathComponent tree)
	parent (.getParent chosen)
	idx (.getIndex parent chosen)
	decorator (tree-node type "new")]
    (if-not (nil? chosen)
      (do 
	(.removeNodeFromParent model chosen)
	(.insert decorator chosen 0)
	(.insertNodeInto model decorator parent idx)))
    (tree-modified tree)))

(defn dispose-frame-action [event tree]
  (.dispose (SwingUtilities/getRoot tree)))

;;
;; toolbar
;;

(defn button 
  ([i s f tree]
     (doto (JButton.)
       (.setToolTipText s)
       (.putClientProperty "JButton.buttonType" "gradient")
       (.setIcon (image-icon i))
       (add-action-listener f tree)))
  ([i s f tree ccp]
     (doto (JButton.)
       (.setToolTipText s)
       (.putClientProperty "JButton.buttonType" "gradient")
       (.setIcon (image-icon i))
       (add-action-listener f tree ccp))))

(defn toolbar [tree ccp]
  (let [toolbar (JToolBar. )]
    (doto toolbar
      (.add (button "new.png" "New" new-action tree))
      (.add (button "open.png" "Open" open-action tree))
      (.add (button "save.png" "Save" save-action tree))
      (.add (button "save-as.png" "Save As"  save-as-action tree))
      (.add (button "export.png" "Export to Graphviz" export-action tree))
      (.addSeparator)
      (.add (button "copy.png" "Copy Node" copy-action tree ccp))
      (.add (button "cut.png" "Cut Node" cut-action tree ccp))
      (.add (button "paste.png" "Paste Node" paste-action tree ccp))
      (.addSeparator)
      (.add (button "expand-tree.png" "Expand Tree" 
      		    expand-tree-action tree))
      (.addSeparator)
      (.add (button "up-arrow.png" "Move Node Up" move-up-action tree))
      (.add 
       (button "down-arrow.png" "Move Node Down" move-down-action tree)))))

(defn filter-nodes
  ([tree type]
     (filter #(= (:type %) type) 
	     (filter-nodes (-> tree .getModel .getRoot))))
  ([node]
     (let [children (.children node)
	   node (.getUserObject node)] 
       (reduce (fn[h v] (clojure.set/union h (filter-nodes v))) 
	       #{node} (enumeration-seq children)))))

(defn popup [tree]
  (let [chosen (.getLastSelectedPathComponent tree)
	{status :status} (.getUserObject chosen)
	item (fn 
	       ([s f tree type]
		  (doto (JMenuItem. s)
		    (add-action-listener f tree type)))
	       ([s f tree]
		    (doto (JMenuItem. s)
		      (add-action-listener f tree))))
	popup (JPopupMenu.)
	insert-menu (JMenu. "Insert")
	actions-menu (JMenu. "Defined Actions")
	decorate-menu (JMenu. "Decorate Node With")]

    (doseq [[type {name :name}] 
	    (filter #(= :action (:type (second %))) node-types)]
      (.add insert-menu (item name insert-action tree type)))

    (doseq [action (filter-nodes tree :action)]
      (.add actions-menu 
	    (item (:name action) insert-defined-action tree action)))
    (.add insert-menu actions-menu)

    (.addSeparator insert-menu)
    (doseq [[type {name :name}] 
	    (filter #(= :composite (:type (second %))) node-types)]
      (.add insert-menu (item name insert-action tree type)))
    (.addSeparator insert-menu)
    (doseq [[type {name :name}] 
	    (filter #(= :decorator (:type (second %))) node-types)]
      (.add insert-menu (item name insert-action tree type)))

    (doseq [[type {name :name}] 
	    (filter #(= :decorator (:type (second %))) node-types)]
      (.add decorate-menu (item name decorate-with-action tree type)))

    (doto popup
      (.add insert-menu)
      (.add decorate-menu)
      (.add (item "Edit" edit-action tree))
      (.addSeparator)
      (.add (if (or (nil? status)
      		    (= :disable status))
      	      (item "Disable" disable-node-action tree)
      	      (item "Enable" enable-node-action tree)))
      (.add (item "Remove" remove-action tree)))))

(defn mouse-adapter [tree]
  (let [show #(.show (popup tree) (.getComponent %1) (.getX %1) (.getY %1))] 
    (proxy [MouseAdapter] []
      (mousePressed [e] (if (.isPopupTrigger e) (show e)))
      (mouseReleased [e] (if (.isPopupTrigger e) (show e))))))

(defn cell-icon [type status]
  (if (and (not (nil? status))
	     (= status :disabled)) 
    (image-icon "disabled.png")
    (image-icon (:icon (node-types type)))))

(defn cell-renderer []
  (doto
      (proxy [DefaultTreeCellRenderer] []
	(getTreeCellRendererComponent
	 [tree value selected expanded leaf row has-focus?]
	 (let [{type :type n :name status :status} (.getUserObject value)]
	   (if (and (not (nil? status))
		    (= status :disabled))
	     (.setText this (str n " (" (name status) "/" (name type) ")"))
	     (.setText this (str n " (" (name type) ")")))
	   (.setOpaque this true)
	   (.setIcon this (cell-icon type status))
	   (.setIconTextGap this 10)
	   (.setBackground this java.awt.Color/white)
	   (.setFont this (java.awt.Font. "Arial" java.awt.Font/PLAIN 15))
	   (if selected
	     (.setBorder this 
			 (LineBorder. (java.awt.Color. 147 157 195) 3 true))
	     (.setBorder this nil))
	   this)))
    (.setLeafIcon (image-icon "edit.png"))
    (.setClosedIcon (image-icon "edit.png"))
    (.setOpenIcon (image-icon "edit.png"))))

(defn cell-editor [tree renderer]
  (proxy [DefaultTreeCellEditor] [tree renderer]
    (isCellEditable [e] (proxy-super isCellEditable e))
    (getTreeCellEditorComponent
     [tree val selected? expanded? leaf row]
     (doto (proxy-super getTreeCellEditorComponent 
			tree val selected? expanded? leaf row)
       (.setFont (java.awt.Font. "Arial" java.awt.Font/PLAIN 15))))))

(defn tree-model [root]
 (proxy [DefaultTreeModel] [root] 
   (valueForPathChanged 
    [path val] 
    (.setUserObject (.getLastPathComponent path) (read-string val)))))

(defn tree [node]
  (let [model (tree-model node)
	tree (JTree. model)
	renderer (cell-renderer)]
    (.setSelectionMode 
     (.getSelectionModel tree) 
     javax.swing.tree.TreeSelectionModel/CONTIGUOUS_TREE_SELECTION)
    (doto tree
      (.setEditable true)
      (.setRowHeight 30)
      (.setShowsRootHandles true)
      (.setCellRenderer renderer)
      (.setCellEditor (cell-editor tree renderer))
      (.addMouseListener (mouse-adapter tree))
      (.setSelectionRow 0))))

(defn key-bindings [event tree ccp]
  (let [modifier (.getModifiers event)
	key (.getKeyChar event)] 
    (when (= 4 modifier) ;;alt pressed
      (cond (= key \n) (new-action event tree)
	    (= key \o) (open-action event tree)
	    (= key \s) (save-action event tree)
	    (= key \e) (edit-action event tree)
	    (= key \v) (paste-action event tree ccp)
	    (= key \x) (cut-action event tree ccp)
	    (= key \c) (copy-action event tree ccp)
	    (= key \w) (dispose-frame-action event tree)))))

(defn frame [node & args]
  (let [[parent-component] args
	panel (JPanel. (BorderLayout.))
	ccp (ref [])
	tree (tree node)
	toolbar (toolbar tree ccp)
	meta (meta (.getUserObject node))
	title (if (nil? meta) "scratch" (.getName (:file meta)))
	frame (JFrame. title)]
    (add-key-typed-listener tree key-bindings tree ccp)
    (doto panel
      (.add toolbar BorderLayout/NORTH)
      (.add (JScrollPane. tree) BorderLayout/CENTER))
    (doto frame
      (.add panel)
      (.pack)
      (.setLocationRelativeTo parent-component)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setVisible true))
    (.requestFocus tree)
    frame))

(defn -main [& args]
  (let [frame #(frame (tree-node :selector "Root"))] 
    (SwingUtilities/invokeLater frame)))
