(defproject alter-ego "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [lein-run "1.0.0-SNAPSHOT"]
		 [org.clojars.nakkaya/miglayout "3.7.3.1"]]
  :run-aliases {:editor [alter-ego.gui.editor editor]})
