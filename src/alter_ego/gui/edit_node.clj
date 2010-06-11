(ns alter-ego.gui.edit-node
  (:use [clojure.contrib.swing-utils :only [add-action-listener]])
  (:import (net.miginfocom.swing MigLayout)
	   (javax.swing JPanel JLabel JTextField JButton)))

(defn selector-panel [frame tree node]
  (let [obj (.getUserObject node)
	name-field (JTextField. (:name obj) 15)
	ok-button (JButton. "Ok")
	cancel-button (JButton. "Cancel")]

    (add-action-listener cancel-button (fn[_] (.setVisible frame false)))
    (add-action-listener ok-button
			 (fn[_]
			   (.setUserObject 
			    node (assoc obj :name (.getText name-field)))
			   (-> tree .getModel (.nodeChanged node))
			   (.setVisible frame false)))

    (doto (JPanel. (MigLayout. "" "[right]"))
      (.add (JLabel. "Edit Selector") "split, span, gaptop 10")
      (.add (javax.swing.JSeparator.) "growx, wrap, gaptop 10")
      (.add (JLabel. "Name") "gap 10")
      (.add name-field "span, growx, wrap paragraph")
      (.add cancel-button "")
      (.add ok-button ""))))

(defn sequence-panel [frame tree node]
  (let [obj (.getUserObject node)
	name-field (JTextField. (:name obj) 15)
	ok-button (JButton. "Ok")
	cancel-button (JButton. "Cancel")]

    (add-action-listener cancel-button (fn[_] (.setVisible frame false)))
    (add-action-listener ok-button
			 (fn[_]
			   (.setUserObject 
			    node (assoc obj :name (.getText name-field)))
			   (-> tree .getModel (.nodeChanged node))
			   (.setVisible frame false)))

    (doto (JPanel. (MigLayout. "" "[right]"))
      (.add (JLabel. "Edit Sequence") "split, span, gaptop 10")
      (.add (javax.swing.JSeparator.) "growx, wrap, gaptop 10")
      (.add (JLabel. "Name") "gap 10")
      (.add name-field "span, growx, wrap paragraph")
      (.add cancel-button "")
      (.add ok-button ""))))

(defn action-panel [frame tree node]
  (let [obj (.getUserObject node)
	name-field (JTextField. (:name obj) 15)
	func-field (JTextField. (:function obj) 15)
	ok-button (JButton. "Ok")
	cancel-button (JButton. "Cancel")]

    (add-action-listener cancel-button (fn[_] (.setVisible frame false)))
    (add-action-listener ok-button
			 (fn[_]
			   (.setUserObject 
			    node (assoc obj 
				   :name (.getText name-field)
				   :function (.getText func-field)))
			   (-> tree .getModel (.nodeChanged node))
			   (.setVisible frame false)))

    (doto (JPanel. (MigLayout. "" "[right]"))
      (.add (JLabel. "Edit Action") "split, span, gaptop 10")
      (.add (javax.swing.JSeparator.) "growx, wrap, gaptop 10")
      (.add (JLabel. "Name") "gap 10")
      (.add name-field "span, growx")
      (.add (JLabel. "Action") "gap 10")
      (.add func-field "span, growx, wrap paragraph")
      (.add cancel-button "")
      (.add ok-button ""))))

(defn edit-node [tree node]
  (let [{type :type} (.getUserObject node)
	frame (javax.swing.JFrame. "Edit Node")]
    (doto frame
      (.add
       (cond (= type :selector) (selector-panel frame tree node)
	     (= type :sequence) (sequence-panel frame tree node)
	     (= type :action) (action-panel frame tree node)))
      (.pack))))

(comment
  (def atree (javax.swing.JTree.))
  (def anode (javax.swing.tree.DefaultMutableTreeNode. 
	      {:type :selector :name "some"}))
  (def anode (javax.swing.tree.DefaultMutableTreeNode. 
	      {:type :action :name "some"}))

  (.setVisible (edit-node atree anode) true)
)
