(ns ^:no-doc jaudy.route.impl
  #?(:cljs (:require-macros [jaudy.route.impl]))
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [jaudy.exception :as ex]
   [jaudy.route.trie :as trie])
  #?(:clj
     (:import
      [java.util HashMap Map]
      [java.net URLEncoder URLDecoder])))

(defn parse [path]
  (let [template #?(:clj (.intern ^String path) :cljs path)
        segments (trie/split-path template)
        variables (->> segments (remove string?) (map :value) set)
        template-key? (comp variables key)]
    {:variables variables
     :segments segments
     :template template}))

(defn wild-route? [{:route/keys [path]}]
  (-> path parse :variables seq boolean))

(defn maybe-map-values
  "Applies a function to every value of a map, updates the value if not nil.
  Also works on vectors. Maintains key for maps, order for vectors."
  [f coll]
  (reduce-kv
    (fn [coll k v]
      (if-some [v' (f v)]
        (assoc coll k v')
        coll))
    coll
    coll))

(defn path-conflicting-routes [routes]
  (or (-> routes meta ::path-conflicting)
      (let [parts-and-routes (mapv (fn [{:route/keys [path] :as r}]
                                     [(trie/split-path path) r])
                                   routes)]
        (not-empty
         (into {}
               (comp (map-indexed (fn [index [p r]]
                                    [r (reduce
                                        (fn [acc [p' r']]
                                          (if (trie/conflicting-parts? p p')
                                            (conj acc r') acc))
                                        #{}
                                        (subvec parts-and-routes (inc index)))]))
                     (filter (comp seq second)))
               parts-and-routes)))))

(defn unresolved-conflicts [path-conflicting]
  (not-empty
   (into {}
         (remove (fn [[route conflicts]]
                   (and (:route/conflict route)
                        (every? :route/conflict conflicts))))
         path-conflicting)))

(defn conflicting-paths [conflicts]
  (->> (for [[p pc] conflicts]
         (conj (map first pc) (first p)))
       (apply concat)
       (set)))

(defn name-conflicting-routes [routes]
  (some->> routes
           (group-by :route/id)
           (remove (comp nil? first))
           (filter (comp pos? count butlast second))
           (seq)
           (map (fn [[k v]] [k (set v)]))
           (into {})))

(defn no-id-conflicts
  [routes]
  (when-let [name-conflicting (name-conflicting-routes routes)]
    (ex/fail! :name-conflicts name-conflicting)))

(defn path-for [route values]
  (if (-> route :variables not-empty)
    (when-let [parts (reduce
                      (fn [acc part]
                        (if (string? part)
                          (conj acc part)
                          (if-let [p (get values (:value part))]
                            (conj acc p)
                            (reduced nil))))
                      []
                      (:segments route))]
      (apply str parts))
    (:template route)))

(defn route-data
  "Return the data to associate with a match."
  [route]
  (or (:route/data route)
      route))

(defn throw-on-missing-values [template variables values]
  (when-not (every? #(contains? values %) variables)
    (let [defined (-> values keys set)
          missing (set/difference variables defined)]
      (ex/fail!
        (str "missing values for route " template " -> " missing)
        {:values values, :variables variables}))))

(defn fast-assoc
  #?@(:clj  [[^clojure.lang.Associative a k v] (.assoc a k v)]
      :cljs [[a k v] (assoc a k v)]))

(defn fast-map [m]
  #?(:clj  (let [m (or m {})] (HashMap. ^Map m))
     :cljs m))

(defn fast-get
  #?@(:clj  [[^HashMap m k] (.get m k)]
      :cljs [[m k] (m k)]))

(defn strip-nils [m]
  (->> m (remove (comp nil? second)) (into {})))

#?(:clj (def +percents+ (into [] (map #(format "%%%02X" %) (range 0 256)))))

#?(:clj (defn byte->percent [^long byte]
          (nth +percents+ (if (< byte 0) (+ 256 byte) byte))))

#?(:clj (defn percent-encode [^String s]
          (->> (.getBytes s "UTF-8") (map byte->percent) (str/join))))

;;
;; encoding & decoding
;;

;; + is safe, but removed so it would work the same as with js
(defn url-encode [s]
  (when s
    #?(:clj  (str/replace s #"[^A-Za-z0-9\!'\(\)\*_~.-]+" percent-encode)
       :cljs (js/encodeURIComponent s))))

(defn maybe-url-decode [s]
  (when s
    #?(:clj  (when (.contains ^String s "%")
               (URLDecoder/decode
                (if (.contains ^String s "+")
                  (.replace ^String s "+" "%2B")
                  ^String s)
                "UTF-8"))
       :cljs (js/decodeURIComponent s))))

(defn url-decode [s]
  (or (maybe-url-decode s) s))

(defn form-encode [s]
  (when s
    #?(:clj  (URLEncoder/encode ^String s "UTF-8")
       :cljs (str/replace (js/encodeURIComponent s) "%20" "+"))))

(defn form-decode [s]
  (when s
    #?(:clj  (if (or (.contains ^String s "%") (.contains ^String s "+"))
               (URLDecoder/decode ^String s "UTF-8")
               s)
       :cljs (js/decodeURIComponent (str/replace s "+" " ")))))

(defn url-decode-coll
  "URL-decodes maps and vectors"
  [coll]
  (maybe-map-values maybe-url-decode coll))

(defprotocol IntoString
  (into-string [_]))

(extend-protocol IntoString
  #?(:clj  String
     :cljs string)
  (into-string [this] this)

  #?(:clj  clojure.lang.Keyword
     :cljs cljs.core.Keyword)
  (into-string [this]
    (let [ns (namespace this)]
      (str ns (when ns "/") (name this))))

  #?(:clj  Boolean
     :cljs boolean)
  (into-string [this] (str this))

  #?(:clj  Number
     :cljs number)
  (into-string [this] (str this))

  #?(:clj  Object
     :cljs object)
  (into-string [this] (str this))

  nil
  (into-string [_]))

(defn path-params
  "Convert parameters' values into URL-encoded strings, suitable for URL paths"
  [params]
  (maybe-map-values #(url-encode (into-string %)) params))

(defn- query-parameter [k v]
  (str (form-encode (into-string k))
       "="
       (form-encode (into-string v))))

(defn query-string
  "shallow transform of query parameters into query string"
  [params]
  (->> params
       (map (fn [[k v]]
              (if (or (sequential? v) (set? v))
                (str/join "&" (map query-parameter (repeat k) v))
                (query-parameter k v))))
       (str/join "&")))

(defmacro goog-extend [type base-type ctor & methods]
  `(do
     (def ~type (fn ~@ctor))

     (goog/inherits ~type ~base-type)

     ~@(map
         (fn [method]
           `(set! (.. ~type -prototype ~(symbol (str "-" (first method))))
                  (fn ~@(rest method))))
         methods)))

(defn ->route-url
  [{:keys [template segments variables] :as route}]
  (fn [values]
    (or (path-for route values)
        (throw-on-missing-values template variables values))))

(defn linear-router
  [routes]
  (let [compiler (trie/compiler)
        [pl nl] (reduce
                 (fn [[pl nl] {:route/keys [id path] :as r}]
                   (let [{:keys [variables] :as route} (parse path)]
                     [(conj pl (-> (trie/insert nil path (route-data r))
                                   (trie/compile)))
                      (if id (assoc nl id (->route-url route)) nl)]))
                 [[] {}]
                 routes)
        matcher (trie/linear-matcher compiler pl true)]
    {:index (fast-map nl)
     :match (trie/path-matcher matcher compiler)}))

(defn lookup-router
  [routes]
  (let [[pl nl]
        (reduce (fn [[pl nl] {:route/keys [id path] :as r}]
                  [(assoc pl path (trie/->Match {} (route-data r)))
                   (if id
                     (assoc nl id (constantly path))
                     nl)])
                [{} {}]
                routes)]
    {:index (fast-map nl)
     :match (fast-map pl)}))

(defn trie-router
  ([routes]
   (let [compiler (trie/compiler)
         [pl nl] (reduce
                  (fn [[pl nl] {:route/keys [id path] :as r}]
                    (let [route (parse path)]
                      [(trie/insert pl path (route-data r))
                       (if id (assoc nl id (->route-url route)) nl)]))
                  [nil {}]
                  routes)
         matcher (trie/compile pl compiler)]
     {:index (fast-map nl)
      :match (trie/path-matcher matcher compiler)})))

(defn expand-or-fail!
  ([route-url id]
   (if route-url
     (route-url nil)
     (ex/fail! :missing-route {:data id})))
  ([route-url id params]
   (if route-url
     (route-url (path-params params))
     (ex/fail! :missing-route {:data id}))))