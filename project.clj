(defproject troclj "0.1.0-SNAPSHOT"
            :description "An experimental Economics Engine in clojure"
            :url "https://github.com/Arnauld/troclj"
            :license {:name "Eclipse Public License"
                      :url  "http://www.eclipse.org/legal/epl-v10.html"}
            :global-vars {
                          ; *warn-on-reflection* true -- disabled because of transitive warnings...
                          *assert* false}
            :dependencies [[org.clojure/clojure "1.6.0"]
                           ;-- web...
                           [ring/ring-core "1.1.8"]
                           [compojure "1.1.5"]
                           [org.clojure/data.json "0.2.1"]
                           [hiccup "1.0.0"]
                           [http-kit "2.1.16"]
                           ;-- misc
                           [org.clojure/tools.logging "0.2.6"]
                           [ch.qos.logback/logback-classic "1.0.13"]]
            :main troclj.web
            :plugins [[lein-cloverage "1.0.2"]])
