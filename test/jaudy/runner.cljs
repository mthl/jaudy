(ns jaudy.runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [clojure.test.check]
   [clojure.test.check.properties]
   [jaudy.route-test]))

(doo-tests 'jaudy.route-test)
