(defproject space-vectors "1.0.0"
  :description "Repl for fast calculations on vectors, lines and planes in space."
  :url "https://github.com/andreasfrom"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.2.8"]
                 [seesaw "1.4.4"]]
  :aot :all
  :main space-vectors.gui)
