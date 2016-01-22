(ns com.gfredericks.schema-bijections-test
  (:require [camel-snake-kebab.core :as csk]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.schema-bijections :as sb :refer :all]
            [schema.core :as s]
            [schema.experimental.generators :as sgen]))

(def camelize-keys (transform-keys csk/->camelCase))

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

(deftest collision-exception-test
  (is (thrown? clojure.lang.ExceptionInfo
               (jsonify-schema {:foo s/Int (s/required-key "foo") s/Uuid}))))

(deftest top-level-errors-test
  (let [my-schema [{:name s/Str
                    :comments [{:created-at s/Int
                                :text s/Str}]}]
        {:keys [from-json to-json]} (jsonify-schema my-schema)

        from-ex (try (to-json [{:name "Joe"
                                :comments [{:created-at 19
                                            :text "Somebody is wrong on the internet."}
                                           {:created-ats 42
                                            :text "Welp I don't know"}]}])
                     (is false "to-json didn't throw!")
                     (catch Exception e e))
        to-ex  (try (from-json [{"name" "Joe"
                                 "comments" [{:created-at 19
                                              "text" "Somebody is wrong on the internet."}
                                             {"createdAt" 42
                                              "text" "Here goes nothing"}]}])
                    (is false "to-json didn't throw!")
                    (catch Exception e e))]
    (is (re-find #"created-at missing-required-key" (pr-str from-ex)))
    (is (re-find #"\"createdAt\" missing-required-key" (pr-str to-ex)))))

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
  (gen/one-of [(gen/elements [s/Bool
                              s/Int
                              Double
                              Long
                              s/Str
                              s/Uuid])
               (gen/let [kws (gen/set gen/keyword {:min-elements 1})]
                 (apply s/enum kws))]))

(def gen-schema
  ;; have to scale this since recursive-gen's sizing is still out of control
  (gen/scale #(min % 15)
             (gen/recursive-gen (fn [inner-gen]
                                  (gen/one-of [(gen/let [[one-schemas [rest-schema]]
                                                         (gen/tuple (gen/vector inner-gen)
                                                                    (gen/vector inner-gen 0 1))]
                                                 (cond-> (mapv #(s/one % "a-schema") one-schemas)
                                                   rest-schema
                                                   (conj rest-schema)))
                                               (gen/let [static-keys
                                                         (gen/map gen/keyword inner-gen)

                                                         dynamic-keys
                                                         (gen/one-of
                                                          [(gen/return nil)
                                                           (gen/tuple inner-gen inner-gen)])]
                                                 (cond-> static-keys
                                                   dynamic-keys
                                                   (conj dynamic-keys)))

                                               (gen/let [schema inner-gen]
                                                 (s/maybe schema))]))
                                gen-leaf-schema)))

(def gen-transformers
  (gen/vector-distinct
   (gen/elements [stringify-uuids
                  stringify-keys
                  stringify-bigdecimals
                  camelize-keys
                  stringify-keyword-enums])))

(def roundtrip-scenario
  (gen/such-that
   (fn [{:keys [schema transformers]}]
     (try (schema->bijection schema transformers)
          (catch clojure.lang.ExceptionInfo e
            (when-not (or (= ::sb/bad-input (:type (ex-data e)))
                          (= ::sb/bad-input (:type (ex-data (.getCause e)))))
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
