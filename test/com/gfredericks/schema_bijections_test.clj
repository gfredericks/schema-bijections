(ns com.gfredericks.schema-bijections-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.schema-bijections :refer :all]
            [schema.core :as s]))

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
        (schema->bijection my-schema [stringify-bigdecimals])

        my-value {:id #uuid "bcd82436-7962-4208-b460-853f5dd75d91"
                  :the-amount 42.9M}]
    (is (s/validate my-schema my-value))
    (is (= (right->left my-value)
           (assoc my-value :the-amount "42.9M")))
    (is (= my-value (-> my-value right->left left->right)))))
