(defproject courjera "0.1.0-SNAPSHOT"
  :description "Clojure wrapper for Coursera Private API"
  :url "https://github.com/davoclavo/courjera"
  :license {:name "MIT License"
            :url "https://github.com/davoclavo/courjera/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "1.0.1"]
                 [org.clojure/data.json "0.2.5"]
                 [clj-time "0.9.0"]]

  :repl-options {:init-ns courjera.core})
