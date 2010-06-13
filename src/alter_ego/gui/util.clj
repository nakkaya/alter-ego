(ns alter-ego.gui.util
  (:import (javax.swing ImageIcon)))

(defn add-action-listener
  "Adds an ActionLister to component. When the action fires, f will be
  invoked with the event as its first argument followed by args.
  Returns the listener."
  [component f & args]
  (let [listener (proxy [java.awt.event.ActionListener] []
                   (actionPerformed [event] (apply f event args)))]
    (.addActionListener component listener)
    listener))

(defn image-icon [file]
  (if-let[res (ClassLoader/getSystemResource file)] 
    (ImageIcon. res)
    (ImageIcon. (ClassLoader/getSystemResource (str "resources/" file)))))

