(ns com.gfredericks.schema-bijections
  (:require [plumbing.core :refer [for-map map-from-keys map-keys]]
            [schema.core :as s]))

(defmacro ^:private throw-bijection-schema-error
  [data]
  `(throw (ex-info "Bijection schema error!"
                   {:type ::bijection-schema-error
                    :data ~data})))

(defn ^:private get-or-throw
  [m k]
  (if-let [[_ v] (find m k)]
    v
    (throw-bijection-schema-error k)))

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

(defn ^:private walk
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
  (wrap-top-level-errors
   (walk schema (transformers->bijector transformers))))

;;
;; The builtin transformers
;;

(defn ^:private non-record-map?
  [m]
  (and (map? m) (not (record? m))))

;; TODO: how should the rest schema play with this?
(defn transform-keys
  "Given a function for transforming map keys, returns a transformer
  that transforms keys of map schemas."
  [func]
  (fn [right]
    (when (non-record-map? right)
      (assert (not-any? #(satisfies? schema.core/Schema %) (keys right))
              "General map schemas not supported yet!")
      (let [bare-keys (map s/explicit-schema-key (keys right))
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
            left (map-keys (fn [k]
                             (cond (keyword? k)
                                   (s/required-key (k->fk k))

                                   (s/optional-key? k)
                                   (s/optional-key (k->fk (:k k)))

                                   (s/required-key? k)
                                   (s/required-key (k->fk (:k k)))

                                   :else
                                   (throw (ex-info "Unknown key" {:k k}))))
                           right)
            left->right (fn [left-obj]
                          (map-keys (fn [k]
                                      (get-or-throw fk->k k))
                                    left-obj))
            right->left (fn [right-obj]
                          (map-keys (fn [k]
                                      (get-or-throw k->fk k))
                                    right-obj))]
        {:left left
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
  "Not technically a bijection anymore I guess."
  [key-schema right]
  (when (and (non-record-map? right)
             (not-any? #(satisfies? s/Schema %) (keys right)))
    (let [right-keys (map s/explicit-schema-key (keys right))]
      {:left (assoc right key-schema s/Any)
       :left->right (fn [left-obj]
                      (select-keys left-obj right-keys))
       :right->left identity})))

(def stringify-keys
  "A transformer that converts keyword keys of maps into string keys."
  (transform-keys name))
