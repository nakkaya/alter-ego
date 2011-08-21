(ns alter-ego.gui.io-test
  (:use [alter-ego.gui.io] :reload-all)
  (:use [clojure.test])
  (:import (javax.swing JTree) 
	   (javax.swing.tree DefaultMutableTreeNode)))

(def sample-tree
     (let [node #(DefaultMutableTreeNode. {:type %1 :name %2})
	   action #(DefaultMutableTreeNode. 
		     {:type :action :name %1 :action %2})
	   root (node :selector "Root")
	   tree (JTree. root)
	   seq1 (node :sequence "Open Door")
	   seq2 (node :sequence "Closed Door")]
       (doto (-> tree .getModel)
       	 (.insertNodeInto seq1 root 0)
       	 (.insertNodeInto seq2 root 1)
         ;;seq1
         (.insertNodeInto (action "Door Open?" 'door-open?) seq1 0)
         (.insertNodeInto (action "Move" 'move) seq1 1)
         ;;seq2
         (.insertNodeInto (action "Move" 'move) seq2 0)
         (.insertNodeInto (action "Open Door" 'open) seq2 1)
         (.insertNodeInto (action "Move" 'move) seq2 2))))

(deftest save-test
  (let [tree (save-tree (.getRoot sample-tree))] 
    (is (= "Root" (-> tree :name)))
    (is (= "Open Door" (->> tree :children first :name)))
    (is (= "Closed Door" (->> tree :children second :name)))
    (is (= "Door Open?" (->> tree :children first :children first :name)))
    (is (= "Move" (->> tree :children second :children first :name)))))

(deftest load-test
  (let [raw (save-tree (.getRoot sample-tree))
	tree (load-tree raw)]
    (is (= "Root" (-> tree .getRoot .getUserObject :name)))
    (is (= "Open Door" (-> tree .getRoot (.getChildAt 0) .getUserObject :name)))
    (is (= "Closed Door" (-> tree .getRoot (.getChildAt 1) .getUserObject :name)))
    (is (= "Move" (-> tree .getRoot (.getChildAt 1) (.getChildAt 2) .getUserObject :name)))))