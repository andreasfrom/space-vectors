(defproject space-vectors "0.1.0-SNAPSHOT"
  :description "Repl for fast calculations on vectors, lines and planes in space."
  :url "https://github.com/andreasfrom"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.2.3"]
                 [expresso "0.2.0"]
                 [reply "0.2.1"]]
  :main space-vectors.core
  :aot [space-vectors.core])
