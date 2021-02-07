(ns jaudy.exception
  (:require
   [clojure.string :as str]))

(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type) {:type type :data data}))))

(defn get-message [e]
  #?(:clj (.getMessage ^Exception e) :cljs (ex-message e)))

(defmulti format-exception (fn [type _ _] type))

(defn exception [e]
  (let [data (ex-data e)
        message (format-exception (:type data) (get-message e) (:data data))]
    ;; there is a 3-arity version (+cause) of ex-info, but the default repl error message is taken from the cause
    (ex-info message (assoc (or data {}) ::cause e))))

;;
;; Formatters
;;

(defmethod format-exception :default [_ message data]
  (str message (when data
                 (str "\n\n" (pr-str data)))))

(defmethod format-exception :path-conflicts [_ _ conflicts]
  (letfn [(resolve-str [path route-data]
            (str (if (:conflicting route-data) "   " "-> ")
                 path " " (not-empty (select-keys route-data [:conflicting]))))]
    (apply str "Router contains conflicting route paths:\n\n"
           (mapv
             (fn [[{:route/keys [path] :as data} vals]]
               (str (resolve-str path data)
                    "\n"
                    (str/join "\n" (mapv (fn [{:route/keys [path] :as data}]
                                           (resolve-str path data))
                                         vals))
                    "\n\n"))
             conflicts))))

(defmethod format-exception :name-conflicts [_ _ conflicts]
  (apply str "Router contains conflicting route names:\n\n"
         (mapv
           (fn [[name vals]]
             (str name "\n-> " (str/join "\n-> " (mapv first vals)) "\n"))
           conflicts)))
