(defproject fr.reuz/jaudy "0.1.0-SNAPSHOT"
  :description "A URI path router for Clojure(Script)"
  :url "https://github.com/mthl/jaudy"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :managed-dependencies [[fr.reuz/jaudy.route "0.1.0-SNAPSHOT"]
                         [org.clojure/clojure "1.10.2"]
                         [org.clojure/clojurescript "1.10.597"]
                         [org.clojure/test.check "1.1.0"]]
  :profiles
  {:dev {:jvm-opts ^:replace ["-server"]
         :java-source-paths ["route/java-src"]
         :source-paths ["route/src"]
         :test-paths ["route/test"]
         :dependencies [[org.clojure/clojure]
                        [org.clojure/clojurescript]
                        [org.clojure/test.check]]}}
  :plugins [[lein-cljsbuild "1.1.8"]
            [lein-doo "0.1.10"]]
  :aliases {"test-cljs" ["doo" "node" "test" "once"]}
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["route/src"
                                       "route/test"
                                       "test"]
                        :compiler {:output-to "target/js/test.js"
                                   :output-dir "target/js"
                                   :main jaudy.runner
                                   :target :nodejs}}]}
  :repl-options {:init-ns jaudy.core})
