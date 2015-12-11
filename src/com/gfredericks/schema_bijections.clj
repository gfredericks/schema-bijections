(ns com.gfredericks.schema-bijections
  (:require [plumbing.core :refer [for-map map-from-keys map-from-vals map-keys]]
            [schema.core :as s]))

(def ^:private schema? #(satisfies? s/Schema %))

(defmacro ^:private throw-bijection-schema-error
  [data]
  `(throw (ex-info "Bijection schema error!"
                   {:type ::bijection-schema-error
                    :data ~data})))

(defn ^:private key-fmap
  [f k]
  ;; MONADS!
  (cond (keyword? k) (f k)
        (s/optional-key? k) (s/optional-key (f (:k k)))
        (s/required-key? k) (s/required-key (f (:k k)))
        :else (throw (ex-info "WTF?" {:k k}))))

(declare walk)

(defmulti walk*
  "Multimethod that walks a schema with a bijector. Should call walk
  instead of direct recursion.

  It should only be necessary to extend this multimethod to support
  new composite schemas."
  (fn [schema bijector] (type schema)))

(defmethod walk* clojure.lang.IPersistentMap
  [schema bijector]
  ;; if this is ever a problem the solution is probably to create a
  ;; special walk* impl? I'm not sure why it would come up though.
  (assert (not (instance? clojure.lang.IRecord schema))
          "Cannot walk records!")

  (letfn [(walk-map-with-no-key-schemas
            [schema]
            (let [key->bijection (for-map [[k v] schema]
                                   (s/explicit-schema-key k)
                                   (walk v bijector))]
              {:left (with-meta (map-from-keys (comp :left key->bijection s/explicit-schema-key) (keys schema))
                       (meta schema))
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
               :right (with-meta (map-from-keys (comp :right key->bijection s/explicit-schema-key) (keys schema))
                        (meta schema))}))]
    (if-let [[key-schema & more-key-schemas]
             (seq (filter schema? (keys schema)))]
      (do
        (assert (empty? more-key-schemas))
        (let [key-schema-bijection (walk key-schema bijector)
              val-schema-bijection (walk (get schema key-schema) bijector)

              {:keys [left left->right right->left right]}
              (walk-map-with-no-key-schemas (dissoc schema key-schema))

              explicit-keys-on-right (map s/explicit-schema-key (keys right))
              explicit-keys-on-left (map s/explicit-schema-key (keys left))]
          {:left
           (assoc left (:left key-schema-bijection) (:left val-schema-bijection))

           :left->right
           (fn [left-obj]
             (-> left-obj
                 (select-keys explicit-keys-on-left)
                 (left->right)
                 (merge (for-map [[k v] (apply dissoc left-obj explicit-keys-on-left)]
                          ((:left->right key-schema-bijection) k)
                          ((:left->right val-schema-bijection) v)))))

           :right->left
           (fn [right-obj]
             (-> right-obj
                 (select-keys explicit-keys-on-right)
                 (right->left)
                 (merge (for-map [[k v] (apply dissoc right-obj explicit-keys-on-left)]
                          ((:right->left key-schema-bijection) k)
                          ((:right->left val-schema-bijection) v)))))

           :right
           (assoc right (:right key-schema-bijection) (:right val-schema-bijection))}))
      (walk-map-with-no-key-schemas schema))))

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

;; This is the same as the :default impl, but we need it here to avoid
;; all the random schema defrecords getting sucked into the
;; IPersistentMap impl.
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

(defn ^:private wrap-top-level-errors
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

(defn walk
  [schema bijector]
  (bijector (walk* schema bijector)))

(defn ^:private transformers->bijector
  [transformers]
  ;; I feel like I'm overcomplicating something.
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
  ;; I think I need some more intuitive terminology/concepts.
  "Given a schema and a sequence of transformers, returns a bijection.

  A transformer is a function from a schema to nil (when the schema is
  not the sort that the transformer intends to transform) or a map:
    :left         the transformed schema
    :left->right  a function that converts values from the transformed
                  schema to the input schema
    :right->left  a function that converts values from the input schema
                  to the transformed schema

  A bijection is the same as the map returned from a transformer."
  [schema transformers]
  (try
    (wrap-top-level-errors
     (walk schema (transformers->bijector transformers)))
    (catch Throwable e
      (throw (ex-info "Exception while creating schema bijection!"
                      {:schema schema
                       :transformers transformers}
                      e)))))

;;
;; The builtin transformers
;;

(defn ^:private non-record-map?
  [m]
  (and (map? m) (not (record? m))))

(defn transform-keys
  "Given a function for transforming map keys, returns a transformer
  that transforms keys of map schemas. If a map schema has a
  key-schema, it will be ignored."
  [func]
  (fn [right]
    (when (non-record-map? right)
      (let [bare-keys (->> (keys right)
                           (remove schema?)
                           (map s/explicit-schema-key))
            fk->k (for-map [k bare-keys] (func k) k)
            k->fk (for-map [k bare-keys] k (func k))
            _ (when (not= (count fk->k)
                          (count k->fk))
                (throw (ex-info "Collision in transform-keys function!"
                                {:schema right
                                 :func func
                                 :colliding-keys (->> bare-keys
                                                      (group-by func)
                                                      (vals)
                                                      (filter #(< 1 (count %)))
                                                      (first))
                                 :type ::bad-input})))

            extra-keys-schema (->> (keys right) (filter schema?) (first))

            ;; I'm not sure if there's a decent use case for having
            ;; this work; at the moment if we don't have this check,
            ;; the generative test objects that key-stringifying the
            ;; map {"*" false, :* false} is ambiguous.
            _ (when extra-keys-schema
                (when-let [colliding (seq (filter #(nil? (s/check extra-keys-schema %)) (vals k->fk)))]
                  (throw (ex-info "Potential collision in transform-keys function!"
                                  {:schema right
                                   :func func
                                   :schema-for-extra-keys extra-keys-schema
                                   :keys-colliding-with-schema-for-extra-keys colliding
                                   :type ::bad-input}))))

            left (map-keys (fn [k]
                             (cond (keyword? k)
                                   (s/required-key (k->fk k))

                                   (s/optional-key? k)
                                   (s/optional-key (k->fk (:k k)))

                                   (s/required-key? k)
                                   (s/required-key (k->fk (:k k)))

                                   (schema? k)
                                   k

                                   :else
                                   (throw (ex-info "Unknown key type" {:k k}))))
                           right)

            left->right (fn [left-obj]
                          (map-keys (fn [fk]
                                      (if-let [[_ k] (find fk->k fk)]
                                        k
                                        (if (and extra-keys-schema
                                                 (nil? (s/check extra-keys-schema fk)))
                                          fk
                                          (throw (ex-info "Disallowed key!"
                                                          {:schema left :key fk})))))
                                    left-obj))
            right->left (fn [right-obj]
                          (map-keys (fn [k]
                                      (if-let [[_ fk] (find k->fk k)]
                                        fk
                                        (if (and extra-keys-schema
                                                 (nil? (s/check extra-keys-schema k)))
                                          k
                                          (throw (ex-info "Disallowed key!"
                                                          {:schema right :key k})))))
                                    right-obj))]
        {:left (with-meta left (meta right))
         :left->right left->right
         :right->left right->left}))))

(defn stringify-uuids
  "A transformer that stringifys UUID schemas."
  [right]
  (when (= s/Uuid right)
    {:left #"^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$"
     :left->right #(java.util.UUID/fromString %)
     :right->left str}))

(defn allow-extra-keys-on-left
  "Returns a transformer that will allow arbitrary extra map keys
  matching the given schema, and removes them when converting."
  [key-schema]
  (fn [right]
    (when (and (non-record-map? right)
               (not-any? #(satisfies? s/Schema %) (keys right)))
      (let [right-keys (map s/explicit-schema-key (keys right))]
        {:left (assoc right key-schema s/Any)
         :left->right (fn [left-obj]
                        (select-keys left-obj right-keys))
         :right->left identity}))))

(def stringify-keys
  "A transformer that converts static keyword keys of maps into string
  keys."
  (transform-keys name))

(defn stringify-keyword-enums
  "A transformer that converts (s/enum :foo :bar :baz) to
  (s/enum \"foo\" \"bar\" \"baz\")."
  [schema]
  (when (and (instance? schema.core.EnumSchema schema)
             (every? keyword? (:vs schema)))
    (let [left->right (map-from-vals name (:vs schema))
          right->left (map-from-keys name (:vs schema))]
      {:left (apply s/enum (keys left->right))
       :left->right left->right
       :right->left right->left})))
