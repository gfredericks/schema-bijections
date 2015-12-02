(ns com.gfredericks.schema-bijections-test
  (:require [camel-snake-kebab.core :as csk]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.schema-bijections :as sb :refer :all]
            [schema.core :as s]
            [schema.experimental.generators :as sgen]))

(def camelize-keys (partial transform-keys csk/->camelCase))

(defn jsonify-schema
  [schema]
  (let [{:keys [left left->right right->left]}
        (schema->bijection schema
                           [stringify-uuids
                            stringify-keys
                            camelize-keys])]
    {:json-schema left
     :to-json right->left
     :from-json left->right}))

(deftest my-bijection
  (let [my-schema {:foo-bar s/Uuid
                   (s/optional-key :thomas) s/Str
                   :comments [{:text s/Str}]}

        {:keys [json-schema to-json from-json]} (jsonify-schema my-schema)]
    (are [my-obj json-obj]
        (and (s/validate my-schema my-obj)
             (s/validate json-schema json-obj)
             (= my-obj (from-json json-obj))
             (= json-obj (to-json my-obj)))

      {:foo-bar #uuid "bcd82436-7962-4208-b460-853f5dd75d91"
       :comments []}
      {"fooBar" "bcd82436-7962-4208-b460-853f5dd75d91"
       "comments" []}

      {:foo-bar #uuid "bcd82436-7962-4208-b460-853f5dd75d91"
       :thomas "heyo"
       :comments [{:text "hiya"} {:text "it is"}]}
      {"fooBar" "bcd82436-7962-4208-b460-853f5dd75d91"
       "thomas" "heyo"
       "comments" [{"text" "hiya"} {"text" "it is"}]})))

;;
;; Custom bijections
;;

(defn stringify-bigdecimals
  [right]
  (when (= java.math.BigDecimal right)
    {:left #"\d+(\.\d+)?M"
     :left->right (fn [s]
                    (BigDecimal. (subs s 0 (dec (count s)))))
     :right->left pr-str}))


(def my-schema
  {:id s/Uuid
   :the-amount java.math.BigDecimal})

(deftest custom-bijection-test
  (let [{:keys [left left->right right->left]}
        (schema->bijection my-schema [stringify-bigdecimals
                                      stringify-keys])

        my-value {:id #uuid "bcd82436-7962-4208-b460-853f5dd75d91"
                  :the-amount 42.9M}]
    (is (s/validate my-schema my-value))
    (is (= (right->left my-value)
           {"id" #uuid "bcd82436-7962-4208-b460-853f5dd75d91"
            "the-amount" "42.9M"}))
    (is (= my-value (-> my-value right->left left->right)))))


;;
;; Properties
;;

(def gen-leaf-schema
  (gen/one-of [(gen/elements [s/Int
                              Double
                              Long
                              s/Str
                              s/Uuid
                              s/Bool])]))

(def gen-schema
  ;; have to scale this since recursive-gen's sizing is still out of control
  (gen/scale #(min % 15)
             (gen/recursive-gen (fn [inner-gen]
                                  (gen/one-of [ ;; TODO: fancier sequence schemas
                                               (gen/let [schema inner-gen]
                                                 [schema])
                                               ;; TODO: fancier map schemas
                                               (gen/map gen/keyword inner-gen)

                                               (gen/let [schema inner-gen]
                                                 (s/maybe schema))]))
                                gen-leaf-schema)))

(def gen-transformers
  (gen/vector-distinct
   (gen/elements [stringify-uuids
                  stringify-keys
                  stringify-bigdecimals
                  camelize-keys])))

(def roundtrip-scenario
  (gen/such-that
   (fn [{:keys [schema transformers]}]
     (try (schema->bijection schema transformers)
          (catch clojure.lang.ExceptionInfo e
            (when-not (= ::sb/bad-input (:type (ex-data e)))
              (throw e)))))
   (gen/let [[schema transformers] (gen/tuple gen-schema
                                              gen-transformers)
             x (gen/scale #(min % 15) (sgen/generator schema))]
     {:schema schema :transformers transformers :x x})))

(defspec roundtrip-spec
  (prop/for-all [{:keys [schema transformers x]}
                 roundtrip-scenario]
    (let [{:keys [left left->right right->left]}
          (schema->bijection schema transformers)]
      (s/validate schema x)
      (s/validate left (right->left x))
      (= x (-> x right->left left->right)))))
