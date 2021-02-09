(ns jaudy.route
  (:require
   [jaudy.route.impl :as impl]
   [jaudy.route.trie :as trie]))

(defprotocol Router
  "A router provides a bidirectional mapping between a collection of
  routes and a collection of URI paths.

  A route is some arbitrary data associated with a URI path which can
  be templated using '{...}' brackets.  For example the URI path
  '/foo/{baz}' contains a variable 'baz'.

  A unique identifier can be associated with a route in order to
  unambiguously retrieving route data and generating URI links."

  (match
    [this path]
    "Find the route data and params matching `path`.

  Route data form depends on the router implementation and route
  definition.

  When the route associated path is templated, the returned params
  provides an assignment of the variables to the values found in
  `path`. Otherwise the params are always an empty map.")

  (path-for
    [this id]
    [this id path-params]
    "Generate a URI path from a route.

  The `id` uniquely identifies at most one route. When no route
  corresponds to that id, an exception is thrown.

  Each route has a URI path which can be templated meaning containing
  variables syntactically identified with '{...}' brackets.

  When retrieving a templated route a corresponding
  `path-params` variable assignment must be provided otherwise an
  exception is thrown."))

(defn router?
  "Check if an object `x` satifies the [[Router]] protocol"
  [x]
  (satisfies? Router x))

;;; Routers

(defn lookup-router
  "Create a “lookup” [[Router]].

  Such router handles only routes with *static* path. A path is
  *static* when it does **not** contain any variable.

  This limitation on the path form provides performance benefits
  meaning that matching has a computational complexity of O(1)."
  [routes]
  (impl/no-id-conflicts routes)
  (when-let [conflicts (impl/path-conflicting-routes routes)]
    (throw (ex-info "Conflicting paths" conflicts)))
  (when-let [wilds (seq (filter impl/wild-route? routes))]
    (throw 
     (ex-info "can't create a lookup router with wildcard routes"
              {:wilds wilds :routes routes})))
  (let [{:keys [index match]} (impl/lookup-router routes)]
    ^{:type ::router}
    (reify Router
      (match [_ path]
        (impl/fast-get match path))

      (path-for [_ id]
        (impl/expand-or-fail! (impl/fast-get index id) id))

      (path-for [_ id params]
        (impl/expand-or-fail! (impl/fast-get index id) id params)))))

(defn trie-router
  "Creates a special prefix-tree router from resolved routes."
  [routes]
  (impl/no-id-conflicts routes)
  (when-let [conflicts (impl/path-conflicting-routes routes)]
    (throw (ex-info "Conflicting paths" conflicts)))
  (let [{:keys [index match]} (impl/trie-router routes)]
    ^{:type ::router}
    (reify
      Router
      (match [_ url]
        (match url))

      (path-for [_ id]
        (impl/expand-or-fail! (impl/fast-get index id) id))

      (path-for [_ id values]
        (impl/expand-or-fail! (impl/fast-get index id) id values)))))

(defn mixed-router
  "Create a [[Router]] combining a [[lookup-router]] for static routes
  and a [[trie-router]] for wildcard routes."
  [routes]
  (impl/no-id-conflicts routes)
  (when-let [conflicts (impl/path-conflicting-routes routes)]
    (throw (ex-info "Conflicting paths" conflicts)))
  (let [{wilds true, statics false} (group-by impl/wild-route? routes)
        {trie-idx :index trie-matcher :match} (impl/trie-router wilds)
        {static-idx :index static-matcher :match} (impl/lookup-router statics)]
    ^{:type ::router}
    (reify Router
      (match [_ url]
        (or (impl/fast-get static-matcher url)
            (trie-matcher url)))

      (path-for [_ id]
        (let [route-url (or (impl/fast-get static-idx id)
                            (impl/fast-get trie-idx id))]
          (impl/expand-or-fail! route-url id)))

      (path-for [_ id values]
        (let [route-url (or (impl/fast-get static-idx id)
                            (impl/fast-get trie-idx id))]
          (impl/expand-or-fail! route-url id values))))))

;;; Routers handling ambiguous routes.

(defn linear-router
  "Create a “linear” [[Router]].

  Such router handles conflicting route paths when matching. Path
  conflicts are solved by using the first matching route in the
  `routes` collection.

  Such conflict resolution capability come at a performance cost,
  meaning that matching has a computational complexity of O(n) where n
  is the number of routes.

  Beside the performance aspect it is better to avoid ambiguous routes
  in most cases because route order introduce complexity and can
  easily lead to unexpected results."
  [routes]
  (impl/no-id-conflicts routes)
  (let [{:keys [index match]} (impl/linear-router routes)]
    ^{:type ::router}
    (reify
      Router
      (match [_ url]
        (match url))

      (path-for [_ id]
        (impl/expand-or-fail! (impl/fast-get index id) id))

      (path-for [_ id values]
        (impl/expand-or-fail! (impl/fast-get index id) id values)))))

(defn quarantine-router
  "Create a “quarantine” [[Router]] combining a [[mixed-router]] for
  non-conflicting routes and a [[linear-router]] for conflicting
  routes."
  [routes]
  (impl/no-id-conflicts routes)
  (let [conflicting-paths (impl/conflicting-paths (impl/path-conflicting-routes routes))
        conflicting? #(contains? conflicting-paths (first %))
        {conflicting true, non-conflicting false} (group-by conflicting? routes)
        {linear-i :index linear-m :match} (impl/linear-router conflicting)
        {wilds true, statics false} (group-by impl/wild-route? non-conflicting)
        {static-i :index static-m :match} (impl/lookup-router statics)
        {trie-i :index trie-m :match} (impl/trie-router wilds)]
    ^{:type ::router}
    (reify Router
      (match [_ url]
        (or (impl/fast-get static-m url)
            (trie-m url)
            (linear-m url)))

      (path-for [_ id]
        (let [route-url (or (impl/fast-get static-i id)
                            (impl/fast-get trie-i id)
                            (impl/fast-get linear-i id))]
          (impl/expand-or-fail! route-url id)))

      (path-for [_ id values]
        (let [route-url (or (impl/fast-get static-i id)
                            (impl/fast-get trie-i id)
                            (impl/fast-get linear-i id))]
          (impl/expand-or-fail! route-url id values))))))

(defn router
  "Create an optimal [[Router]] from a collection of routes.

  The actual router implementation used is selected based on the
  characteristics of the routes.

  Ambiguous routes must be explicitely marked with the
  `:route/conflict` set to true. In order to handle conflicts
  implicitely use a [[quarantine-router]] instead."
  [routes]
  (let [path-conflicting (impl/path-conflicting-routes routes)
        wilds? (boolean (some impl/wild-route? routes))
        all-wilds? (every? impl/wild-route? routes)
        router (cond
                 path-conflicting quarantine-router
                 (not wilds?) lookup-router
                 all-wilds? trie-router
                 :else mixed-router)]
    (if-let [conflicts (impl/unresolved-conflicts path-conflicting)]
      (throw (ex-info "Conflicting paths" conflicts))
      (router
       (vary-meta routes assoc ::impl/path-conflicting path-conflicting)))))

(defn add-query-params
  "Append some query params `qparams` to a URI `path`.

  The query-params are processed to be URL-encoded."
  [path qparams]
  (cond-> path
    (seq qparams) (str "?" (impl/query-string qparams))))
