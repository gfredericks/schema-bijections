(defproject com.gfredericks/schema-bijections "0.1.1-SNAPSHOT"
  :description "A library for bijecting prismatic schemas."
  :url "https://github.com/gfredericks/schema-bijections"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "1.0.3"]
                 [prismatic/plumbing "0.5.2"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:dependencies [[camel-snake-kebab "0.3.2"]
                                  [org.clojure/test.check "0.9.0"]]}})
