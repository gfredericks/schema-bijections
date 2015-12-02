# schema-bijections

A first draft of a library for bijecting prismatic schemas.

## Obtention

Not yet.

## Rationale

## Examples

``` clojure
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
```


## License

Copyright © 2015 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
