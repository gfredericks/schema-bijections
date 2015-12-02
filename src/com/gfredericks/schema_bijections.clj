(ns com.gfredericks.schema-bijections
  (:require [plumbing.core :refer [for-map map-from-keys map-keys]]
            [schema.core :as s]))

(defmacro throw-bijection-schema-error
  [data]
  `(throw (ex-info "Bijection schema error!"
                   {:type ::bijection-schema-error
                    :data ~data})))

(defn get-or-throw
  [m k]
  (if-let [[_ v] (find m k)]
    v
    (throw-bijection-schema-error k)))

(def Schema "A schema schema" (s/protocol s/Schema))
(def Bijection
  {:left Schema
   :left->right (s/=> s/Any s/Any)
   :right->left (s/=> s/Any s/Any)
   :right Schema})

(s/defn ->bijection :- Bijection
  "Given a schema, returns the identity bijection."
  [schema :- Schema]
  {:left schema
   :left->right identity
   :right->left identity
   :right schema})

(defn key-fmap
  [f k]
  ;; MONADS!
  (cond (keyword? k) (f k)
        (s/optional-key? k) (s/optional-key (f (:k k)))
        (s/required-key? k) (s/required-key (f (:k k)))
        :else (throw (ex-info "WTF?" {:k k}))))

(declare walk)

(defmulti walk* (fn [schema bijector] (type schema)))

(defmethod walk* clojure.lang.IPersistentMap
  [schema bijector]
  ;; if this is ever a problem the solution is probably to create a
  ;; special walk* impl? I'm not sure why it would come up though.
  (assert (not (instance? clojure.lang.IRecord schema))
          "Cannot walk* records!")


  (let [key->bijection (for-map [[k v] schema]
                         (s/explicit-schema-key k)
                         (walk v bijector))]
    {:left (map-from-keys (comp :left key->bijection s/explicit-schema-key) (keys schema))
     :left->right (fn [left-obj]
                    (for-map [[k v] left-obj
                              :let [{:keys [left->right]} (or (get key->bijection k)
                                                              (throw-bijection-schema-error left-obj))]]
                      k (left->right v)))
     :right->left (fn [right-obj]
                    (for-map [[k v] right-obj
                              :let [{:keys [right->left]} (or (get key->bijection k)
                                                              (throw-bijection-schema-error right-obj))]]
                      k (right->left v)))
     :right (map-from-keys (comp :right key->bijection s/explicit-schema-key) (keys schema))}))

(defmethod walk* clojure.lang.IPersistentVector
  [schema bijector]
  (assert (and (= 1 (count schema))
               (satisfies? schema.core/Schema (first schema))
               (not (instance? schema.core.One (first schema))))
          "General sequence schemas not implemented yet!")
  (let [{:keys [left left->right right->left right]} (walk (first schema) bijector)]
    {:left [left]
     :left->right #(mapv left->right %)
     :right->left #(mapv right->left %)
     :right [right]}))

(defmethod walk* schema.core.Maybe
  [schema bijector]
  (let [{:keys [left left->right right->left right]} (walk (:schema schema) bijector)]
    {:left (s/maybe left)
     :left->right #(some-> % left->right)
     :right->left #(some-> % right->left)
     :right (s/maybe right)}))

(defmethod walk* clojure.lang.IRecord
  [schema bijector]
  {:left schema
   :left->right identity
   :right->left identity
   :right schema})

(defmethod walk* :default
  [schema bijector]
  {:left schema
   :left->right identity
   :right->left identity
   :right schema})

(prefer-method walk* clojure.lang.IRecord clojure.lang.IPersistentMap)

(defn walk
  [schema bijector]
  (bijector (walk* schema bijector)))

(defn transformers->bijector
  [transformers]
  (if-let [[tx & txs] (seq transformers)]
    (let [bijector (transformers->bijector txs)]
      (fn [bijection]
        (let [{:keys [left left->right right->left right] :as bijection}
              (bijector bijection)]
          (if-let [{left' :left, left->right' :left->right, right->left' :right->left}
                   (tx left)]
            {:left left'
             :left->right (comp left->right left->right')
             :right->left (comp right->left' right->left)
             :right right}
            bijection))))
    identity))

(defn schema->bijection
  [schema transformers]
  (walk schema (transformers->bijector transformers)))

(defn non-record-map?
  [m]
  (and (map? m) (not (record? m))))

;; TODO: how should the rest schema play with this?
(defn transform-keys
  "Transforms keys of map schemas in the left schema."
  [func right]
  (when (and (non-record-map? right)
             (every? keyword? (map s/explicit-schema-key (keys right))))
    (let [bare-keys (map s/explicit-schema-key (keys right))
          string->keyword (for-map [k bare-keys] (func k) k)
          keyword->string (for-map [k bare-keys] k (func k))
          _ (when (not= (count string->keyword)
                        (count keyword->string))
              (throw (ex-info "Collision in transform-keys function!"
                              {:schema right
                               :func func
                               :colliding-keys (->> bare-keys
                                                    (group-by func)
                                                    (vals)
                                                    (filter #(< 1 (count %)))
                                                    (first))
                               :type ::bad-input})))
          left (map-keys (fn [k]
                            (if (keyword? k)
                              (s/required-key (keyword->string k))
                              (if (s/optional-key? k)
                                (s/optional-key (keyword->string (:k k)))
                                (throw (ex-info "Unknown key" {:k k})))))
                          right)
          left->right (fn [left-obj]
                         (map-keys (fn [k]
                                     (get-or-throw string->keyword k))
                                   left-obj))
          right->left (fn [right-obj]
                         (map-keys (fn [k]
                                     (get-or-throw keyword->string k))
                                   right-obj))]
      {:left left
       :left->right left->right
       :right->left right->left})))

(defn stringify-uuids
  "Stringifys any UUIDs in the left schema."
  [right]
  (when (= s/Uuid right)
    {:left #"^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$"
     :left->right #(java.util.UUID/fromString %)
     :right->left str}))

(defn allow-extra-keys-on-left
  "Not technically a bijection anymore I guess."
  [key-schema right]
  (when (and (non-record-map? right)
             (not-any? #(satisfies? s/Schema %) (keys right)))
    (let [right-keys (map s/explicit-schema-key (keys right))]
      {:left (assoc right key-schema s/Any)
       :left->right (fn [left-obj]
                      (select-keys left-obj right-keys))
       :right->left identity})))

(def stringify-keys (partial transform-keys name))

(defn wrap-top-level-errors
  [{:keys [left left->right right->left right]}]
  {:left left
   :left->right (fn [left-obj]
                  (try
                    (left->right left-obj)
                    (catch Throwable t
                      (throw
                       (if-let [err (s/check left left-obj)]
                         (ex-info "Schema bijection error!"
                                  {:schema left
                                   :data left-obj
                                   :error err})
                         (ex-info "Unknown schema bijection error!"
                                  {:schema left
                                   :data left-obj}
                                  t))))))
   :right->left (fn [right-obj]
                  (try
                    (right->left right-obj)
                    (catch Throwable t
                      (throw
                       (if-let [err (s/check right right-obj)]
                         (ex-info "Schema bijection error!"
                                  {:schema right
                                   :data right-obj
                                   :error err})
                         (ex-info "Unknown schema bijection error!"
                                  {:schema right
                                   :data right-obj}
                                  t))))))
   :right right})

;;
;; Library types
;;

#_#_
(defn stringify-joda-date-times
  "Stringifies any DateTimes in the left schema."
  [right]
  (when (= org.joda.time.DateTime left)
    {:left #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3}Z"
     :left->right clj-time.format/parse
     :right->left str}))

(defn stringify-joda-local-dates
  "Stringifies any LocalDates in the left schema."
  [right]
  (when (= org.joda.time.LocalDate right)
    {:left #"\d{4}-\d{2}-\d{2}"
     :left->right clj-time.format/parse-local-date
     :right->left str}))
