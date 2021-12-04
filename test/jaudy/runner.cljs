(ns jaudy.runner
  (:require
   [clojure.test :refer-macros [run-tests]]
   [clojure.test.check]
   [clojure.test.check.properties]
   [jaudy.route-test]))

(run-tests 'jaudy.route-test)
