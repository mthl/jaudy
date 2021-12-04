(ns build
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.set :as set]
   [clojure.tools.build.api :as b]
   [clojure.tools.deps.alpha.util.dir :as dir]
   [deps-deploy.deps-deploy :as dd]))

(def version "0.1.0-SNAPSHOT")

(defn- module-basis
  [module-root]
  (with-bindings {#'b/*project-root* module-root}
    (b/create-basis)))

(def modules
  #{{:lib 'fr.reuz/jaudy.route
     :basis (module-basis "modules/route")
     :src-dirs ["modules/route/src"]
     :class-dir "modules/route/target/classes"
     :javac-opts ["-source" "8" "-target" "8"]
     :jar-file (format "modules/route/target/jaudy.route-%s.jar" version)}})

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "modules/route/target"}))

(defn compile [_]
  (doseq [opts modules]
    (when (contains? opts :javac-opts)
      (b/javac opts))))

(defn cljs-test [_]
  (let [basis (b/create-basis {:project "deps.edn" :aliases [:test]})
        compilation (-> {:basis basis
                         :main 'clojure.main
                         :main-args ["-m" "cljs.main"
                                     "--target" "node"
                                     "--output-dir" "target/cljs/test"
                                     "--output-to" "target/cljs/run-tests.js"
                                     "-c" "jaudy.runner"]}
                        b/java-command
                        b/process)]
    (when (= 0 (:exit compilation))
      (b/process {:command-args ["node" "target/cljs/run-tests.js"]}))))

(let [module-libs (into #{} (map :lib) modules)]
  (defn- add-version
    [lib-specs]
    (reduce-kv (fn [acc lib spec]
                 (assoc acc lib (cond-> spec
                                  (contains? module-libs lib)
                                  (assoc :mvn/version version))))
               {}
               lib-specs)))

(defn package [_]
  (compile nil)
  (doseq [opts modules]
    (b/write-pom (-> opts
                     (assoc :version version)
                     (update-in [:basis :libs] add-version)))
    (b/copy-dir (assoc opts
                       :ignores [".*\\.java"]
                       :target-dir (:class-dir opts)))
    (b/jar opts)))

(defn install [_]
  (doseq [opts modules]
    (b/install (assoc opts :version version))))

(defn deploy [_]
  (doseq [{:keys [jar-file] :as opts} modules]
    (dd/deploy {:artifact jar-file
                :installer :remote
                :pom-file (b/pom-path opts)
                :sign-releases? true})))
