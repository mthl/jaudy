(ns jaudy.route-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [jaudy.route :as r #?@(:cljs [:refer [Router]])]
   [jaudy.route.impl :as impl])
  #?(:clj
     (:import
      [jaudy.route Router]
      [clojure.lang ExceptionInfo])))

(deftest router-test
  (testing "routers handling wildcard paths"
    (are [router]
        (testing "wild"

          (testing "simple"
            (let [r (router [{:route/path "/api/ipa/{size}"
                              :route/id ::beer}])]
              (is (= {:data {:route/id :default}
                      :params {}}
                     (into {} (r/match r "/api"))))
              (is (= {:data {:route/id ::beer
                             :route/path "/api/ipa/{size}"}
                      :params {:size "large"}}
                     (into {} (r/match r "/api/ipa/large"))))
              (is (= "/api/ipa/large"
                     (r/path-for r ::beer {:size "large"})))
              (is (= "/api/ipa/large"
                     (r/path-for r ::beer {:size :large})))

              (is (thrown-with-msg?
                   ExceptionInfo
                   #"missing route identifier"
                   (r/path-for r "ILLEGAL"))
                  "invalid route identifier bails out")

              (is (thrown-with-msg?
                   ExceptionInfo
                   #"missing variables assignment"
                   (r/path-for r ::beer))
                  "missing parameters bails out")))

          (testing "decode %-encoded path params"
            (let [r (router [{:route/path "/one-param-path/{param1}"
                              :route/id ::one}
                             {:route/path "/two-param-path/{param1}/{param2}"
                              :route/id ::two}
                             {:route/path "/catchall{/remaining-path}"
                              :route/id ::catch-all}])
                  decoded-params #(:params (r/match r %))
                  decoded-param1 #(:param1 (decoded-params %))
                  decoded-remaining-path #(:remaining-path (decoded-params %))]
              (is (= "/one-param-path/k%C3%A4ki" (r/path-for r ::one {:param1 "käki"})))
              (is (= "foo bar" (decoded-param1 "/one-param-path/foo%20bar")))
              (is (= {:param1 "foo bar" :param2 "baz qux"} (decoded-params "/two-param-path/foo%20bar/baz%20qux")))
              (is (= "foo bar" (decoded-remaining-path "/catchall/foo%20bar")))
              (is (= "!#$&'()*+,/:;=?@[]"
                     (decoded-param1 "/one-param-path/%21%23%24%26%27%28%29%2A%2B%2C%2F%3A%3B%3D%3F%40%5B%5D")))))

          (testing "complex"
            (let [r (router [{:route/path "/{abba}"
                              :route/id ::abba}
                             {:route/path "/abba/1"
                              :route/id ::abba2}
                             {:route/path "/{jabba}/2"
                              :route/id ::jabba2}
                             {:route/path "/{abba}/{dabba}/doo"
                              :route/id ::doo}
                             {:route/path "/abba/dabba/boo/baa"
                              :route/id ::baa}
                             {:route/path "/abba/{dabba}/boo"
                              :route/id ::boo}
                             {:route/path "/{jabba}/{dabba}/{doo}/{daa}{/foo}"
                              :route/id ::wild}])
                  by-path #(-> r (r/match %) :data :route/id)]
              (is (= ::abba (by-path "/abba")))
              (is (= ::abba2 (by-path "/abba/1")))
              (is (= ::jabba2 (by-path "/abba/2")))
              (is (= ::doo (by-path "/abba/1/doo")))
              (is (= ::boo (by-path "/abba/1/boo")))
              (is (= ::baa (by-path "/abba/dabba/boo/baa")))
              (is (= ::boo (by-path "/abba/dabba/boo")))
              (is (= ::wild (by-path "/olipa/kerran/avaruus/vaan/")))
              (is (= ::wild (by-path "/olipa/kerran/avaruus/vaan/ei/toista/kertaa")))))

          (testing "bracket-params"
            (testing "successful"
              (let [r (router [{:route/path "/{abba}"
                                :route/id ::abba}
                               {:route/path "/abba/1"
                                :route/id ::abba2}
                               {:route/path "/{jabba}/2"
                                :route/id ::jabba2}
                               {:route/path "/{abba}/{dabba}/doo"
                                :route/id ::doo}
                               {:route/path "/abba/dabba/boo/baa"
                                :route/id ::baa}
                               {:route/path "/abba/{dabba}/boo"
                                :route/id ::boo}
                               {:route/path "/{a/jabba}/{a.b/dabba}/{a.b.c/doo}/{a.b.c.d/daa}{/foo/bar}"
                                :route/id ::wild}
                               {:route/path "/files/file-{name}.html"
                                :route/id ::html}
                               {:route/path "/files/file-{name}.json"
                                :route/id ::json}
                               {:route/path "/{eskon}/{saum}/pium\u2215paum"
                                :route/id ::loru}
                               {:route/path "/{🌈}🤔/🎈"
                                :route/id ::emoji}
                               {:route/path "/extra-end}s-are/ok"
                                :route/id ::bracket}])
                    by-path #(-> r (r/match %) ((juxt (comp :route/id :data) :params)))]
                (is (= [::abba {:abba "abba"}] (by-path "/abba")))
                (is (= [::abba2 {}] (by-path "/abba/1")))
                (is (= [::jabba2 {:jabba "abba"}] (by-path "/abba/2")))
                (is (= [::doo {:abba "abba", :dabba "1"}] (by-path "/abba/1/doo")))
                (is (= [::boo {:dabba "1"}] (by-path "/abba/1/boo")))
                (is (= [::baa {}] (by-path "/abba/dabba/boo/baa")))
                (is (= [::boo {:dabba "dabba"}] (by-path "/abba/dabba/boo")))
                (is (= [::wild {:a/jabba "olipa"
                                :a.b/dabba "kerran"
                                :a.b.c/doo "avaruus"
                                :a.b.c.d/daa "vaan"
                                :foo/bar "ei/toista/kertaa"}]
                       (by-path "/olipa/kerran/avaruus/vaan/ei/toista/kertaa")))
                (is (= [::html {:name "10"}] (by-path "/files/file-10.html")))
                (is (= [::loru {:eskon "viitan", :saum "aa"}] (by-path "/viitan/aa/pium\u2215paum")))
                (is (= [:default {}] (by-path "/ei/osu/pium/paum")))
                (is (= [::emoji {:🌈 "brackets"}] (by-path "/brackets🤔/🎈")))
                (is (= [::bracket {}] (by-path "/extra-end}s-are/ok")))))

          (testing "invalid syntax fails fast"
            (testing "unclosed brackets"
              (is (thrown-with-msg?
                   ExceptionInfo
                   #"unclosed bracket"
                   (r/router [{:route/path "/kikka/{kukka"}]))))

            (testing "multiple terminators"
              (is (thrown-with-msg?
                   ExceptionInfo
                   #"multiple terminators"
                   (r/router [{:route/path "/{kukka}.json"
                               :route/id ::.}
                              {:route/path "/{kukka}-json"
                               :route/id ::-}]))))))

          (testing "empty path segments"
            (let [r (router [{:route/path "/items"
                              :route/id ::list}
                             {:route/path "/items/{id}"
                              :route/id ::item}
                             {:route/path "/items/{id}/{side}"
                              :route/id ::deep}])
                  matches #(-> r (r/match %) :data :route/id)]
              (is (= ::list (matches "/items")))
              (is (= :default (matches "/items/")))
              (is (= ::item (matches "/items/1")))
              (is (= ::deep (matches "/items/1/2")))
              (is (= :default (matches "/items//2")))
              (is (= :default (matches ""))))))

      r/linear-router
      r/trie-router
      r/mixed-router
      r/quarantine-router))

  (testing "routers handling static paths"
    (are [router]
        (let [r (router [{:route/path "/api/ipa/large"
                          :route/id ::beer}])]
          (is (= {:data {:route/id :default}
                  :params {}}
                 (into {} (r/match r "/api"))))
          (is (= {:data {:route/id ::beer
                         :route/path "/api/ipa/large"}
                  :params {}}
                 (into {} (r/match r "/api/ipa/large"))))
          (is (= "/api/ipa/large"
                 (r/path-for r ::beer {:size "large"}))
              "ignore extra path params")
          (is (thrown-with-msg?
               ExceptionInfo
               #"missing route identifier"
               (r/path-for r "ILLEGAL"))))

      r/lookup-router
      r/linear-router
      r/trie-router
      r/mixed-router
      r/quarantine-router)

    (testing "can't be created with wildcard routes"
      (is (thrown-with-msg?
           ExceptionInfo
           #"can't create a lookup router with wildcard routes"
           (r/lookup-router
            [{:route/path "/api/{version}/ping"
              :route/id ::ping}])))))

  (testing "ring sample"
    (let [pong (constantly "ok")
          routes [{:route/path "/api/ping"
                   :mw [:api]
                   :route/id :kikka}
                  {:route/path "/api/user/{id}/{sub-id}"
                   :route/id ::user-id
                   :mw [:api]
                   :parameters {:id "String", :sub-id "String"}}
                  {:route/path "/api/pong"
                   :route/id ::pong
                   :mw [:api]
                   :handler pong}
                  {:route/path "/api/admin/user"
                   :route/id ::user
                   :mw [:api :admin]
                   :roles #{:user}}
                  {:route/path "/api/admin/db"
                   :route/id ::db
                   :mw [:api :admin :db]
                   :roles #{:admin}}]
          router (r/router routes)]
      (is (= {:data {:route/path "/api/user/{id}/{sub-id}"
                     :route/id ::user-id
                     :mw [:api]
                     :parameters {:id "String", :sub-id "String"}}
              :params {:id "1", :sub-id "2"}}
             (into {} (r/match router "/api/user/1/2")))))))

(deftest conflicting-routes-test
  (testing "path conflicts"
    (are [conflicting? routes]
        (let [conflicts (impl/path-conflicting-routes routes)]
          (if conflicting? (seq conflicts) (nil? conflicts)))

      true [{:route/path "/a"}
            {:route/path "/a"}]

      true [{:route/path "/a"}
            {:route/path "/{b}"}]

      true [{:route/path "/a"}
            {:route/path "{/b}"}]

      true [{:route/path "/a/1/2"}
            {:route/path "{/b}"}]

      false [{:route/path "/a"}
             {:route/path "/a/"}]

      false [{:route/path "/a"}
             {:route/path "/a/1"}]

      false [{:route/path "/a"}
             {:route/path "/a/{b}"}]

      false [{:route/path "/a"}
             {:route/path "/a/{b}"}]

      true [{:route/path "/v2/public/messages/dataset/bulk"}
            {:route/path "/v2/public/messages/dataset/{dataset-id}"}])

    (testing "all conflicts are returned"
      (is (= {{:route/path "/a"} #{{:route/path "{/d}"}
                                   {:route/path "/{b}"}}
              {:route/path "/{b}"} #{{:route/path "/c"}
                                     {:route/path "{/d}"}}
              {:route/path "/c"} #{{:route/path "{/d}"}}}
             (impl/path-conflicting-routes
              [{:route/path "/a"}
               {:route/path "/{b}"}
               {:route/path "/c"}
               {:route/path "{/d}"}]))))

    (testing "router with conflicting routes"
      (testing "throws by default"
        (is (thrown-with-msg?
             ExceptionInfo
             #"Conflicting paths"
             (r/router
              [{:route/path "/a"}
               {:route/path "/a"}]))))

      (testing "can be configured to ignore with route data"
        (are [paths]
            (let [router (r/router paths)]
              (is (not (nil? router))))
          [{:route/path "/a" :route/id ::a0 :route/conflict true}
           {:route/path "/a" :route/id ::a1 :route/conflict true}]

          [{:route/path "/a" :route/id ::a :route/conflict true}
           {:route/path "/{b}" :route/id ::b :route/conflict true}
           {:route/path "/c" :route/id ::c :route/conflict true}
           {:route/path "{/d}" :route/id ::d :route/conflict true}]

          [{:route/path "/:a/{b}" :route/id ::ab :route/conflict true}
           {:route/path "/:a/{c}" :route/id ::ac :route/conflict true}
           {:route/path "/:a/{d}/{e}" :route/id ::ade :route/conflict true}
           {:route/path "/:a/{d}/{f}" :route/id ::adf :route/conflict true}]

          [{:route/path "/:a/{b}" :route/id ::ab :route/conflict true}
           {:route/path "/:a/{c}" :route/id ::ac :route/conflict true}
           {:route/path "/:a/{d}/{e}" :route/id ::de :route/conflict true}
           {:route/path "/:a/{d}/{f}" :route/id ::adf :route/conflict true}])

        (testing "unmarked path conflicts throw"
          (are [paths]
              (is (thrown-with-msg?
                   ExceptionInfo
                   #"Conflicting paths"
                   (r/router paths)))
            [{:route/path "/a" :route/id ::a0}
             {:route/path "/a" :route/id ::a1 :route/conflict true}]
            [{:route/path "/a" :route/id ::a0 :route/conflict true}
             {:route/path "/a" :route/id ::a1 }])))))

  (testing "name conflicts"
    (testing "router with conflicting routes always throws"
      (is (thrown-with-msg?
           ExceptionInfo
           #"Duplicated route identifier"
           (r/router
            [{:route/path "/1" :route/id ::1}
             {:route/path "/2" :route/id ::1}]))))))

(deftest path-for-test
  (let [router (r/router [{:route/path "/{a}/{b}"
                           :route/id ::route}])]
    (is (= "/olipa/kerran"
           (r/path-for router ::route {:a "olipa", :b "kerran"})))
    (is (= "/olipa/kerran"
           (r/path-for router ::route {:a "olipa", :b "kerran"})))
    (is (= "/olipa/kerran?iso=p%C3%B6ril%C3%A4inen"
           (r/add-query-params (r/path-for router ::route {:a "olipa", :b "kerran"})
                               {:iso "pöriläinen"})))))

(deftest routing-order-test-229
  (let [router (r/quarantine-router
                [{:route/path "/" :route/id :root}
                 {:route/path "/" :route/id :create :method :post}])
        router2 (r/quarantine-router
                 [{:route/path "{/a}" :route/id :root}
                  {:route/path "/{a}b/c/d" :route/id :create :method :post}])]
    (is (= :root (-> (r/match router "/") :data :route/id)))
    (is (= :root (-> (r/match router2 "/") :data :route/id)))))

(deftest default-route
  (testing "implicit default"
    (let [router (r/router [{:route/path "/{a}/{b}"
                             :route/id ::route}])]
      (is (thrown? ExceptionInfo
                   (r/path-for router :default)))
      (is (= {:data {:route/id :default}
              :params {}}
             (into {} (r/match router "/not-matching"))))))

  (testing "explicit default"
    (let [router (r/router [{:route/path "/{a}/{b}"
                             :route/id ::route}
                            {:route/id :default
                             :example 42}])]
      (is (thrown? ExceptionInfo
                   (r/path-for router :default)))
      (is (= {:data {:route/id :default
                     :example 42}
              :params {}}
             (into {} (r/match router "/not-matching")))))))

(deftest route-data-test
  (testing "route/data value override the data returned when matching a route"
    (let [router (r/router [{:route/path "/{a}/{b}"
                             :route/id ::route
                             :route/data {:example 14}}
                            {:route/id :default
                             :route/data {:example 42}}])]
      (is (= {:data {:example 42}
              :params {}}
             (into {} (r/match router "/not-matching"))))
      (is (= {:data {:example 14}
              :params {:a "matching" :b "path"}}
             (into {} (r/match router "/matching/path")))))))
