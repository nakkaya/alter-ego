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
    (is (= {:type :selector, :name "Root"} (first tree)))
    (is (= {:type :sequence, :name "Open Door"} (first (second tree))))
    (is (= {:type :sequence, :name "Closed Door"} (first (nth tree 2))))
    (is (= {:type :action, :name "Door Open?", :action 'door-open?} 
	   (first (second (second tree)))))
    (is (= {:type :action, :name "Move", :action 'move} 
	   (first (second (nth tree 2)))))))

(deftest load-test
  (let [raw (save-tree (.getRoot sample-tree))
	tree (load-tree (DefaultMutableTreeNode. (first raw)) (rest raw))
	root (.getRoot tree)
	seq2 (.getChildAt root 1)] 
    (is (= {:type :selector, :name "Root"} (.getUserObject root)))
    (is (= {:type :sequence, :name "Open Door"} 
	   (.getUserObject (.getChildAt root 0))))
    (is (= {:type :sequence, :name "Closed Door"} 
	   (.getUserObject seq2)))
    (is (= {:type :action, :name "Move", :action 'move}
	   (.getUserObject (.getChildAt seq2 2))))))
