(defproject fr.reuz/jaudy "0.1.0-SNAPSHOT"
  :description "A URI path router for Clojure(Script)"
  :url "https://github.com/mthl/jaudy"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :java-source-paths ["java-src"]
  :javac-options ["-Xlint:unchecked" "-target" "1.8" "-source" "1.8"]
  :dependencies [[org.clojure/clojure "1.10.2"]]
  :repl-options {:init-ns jaudy.core})
