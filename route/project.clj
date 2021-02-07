(defproject fr.reuz/jaudy.route "0.1.0-SNAPSHOT"
  :scm {:dir ".."}
  :plugins [[lein-parent "0.3.8"]]
  :parent-project {:path "../project.clj"
                   :inherit [:managed-dependencies
                             :deploy-repositories
                             :description
                             :url
                             :license
                             [:profiles :dev :jvm-opts]
                             [:profiles :dev :dependencies]
                             :repl-options]}
  :dependencies [[org.clojure/clojure]]
  :java-source-paths ["java-src"])
