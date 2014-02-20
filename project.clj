(defproject petri_nets "0.1.0-SNAPSHOT"
  :description "Agar: A Clojure Petri-net simulator."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.2"]]
  :main ^:skip-aot petri-nets.gui
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
