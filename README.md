# schema-bijections

A first draft of a library for bijecting prismatic schemas.

## Obtention

```
[com.gfredericks/schema-bijections "0.1.3"]
```

## Rationale

I use [prismatic/schema](https://github.com/Prismatic/schema) a lot,
and was excited when I learned about the coercers feature because I
thought it would solve the problem of using boilerplate or messy
shortcuts to convert data into fluent clojure representations (e.g.,
keyword keys, kebab-cased keys, joda types for timestamps,
`java.util.UUID` instances for UUID's, etc.).

A coercer is a function produced from a schema, which can convert
objects that "almost" match the schema to objects that actually do
match the schema. I had quickly figured out how to use coercers to
solve all of the problems mentioned above, but I quickly noticed some
drawbacks.

My primary use case was for specifying a JSON HTTP API. I used schemas
to describe the input and output data structures, in their
clojure-fluent form. Among the drawbacks:

- Coercers only helped for coercing the input, not the output. For
  output I had to convert key casing myself and hope that the default
  behavior of the json serializer did everything else correctly.
- I didn't have a precise specification of the input or output schemas
  as they looked to the external world. I ended up writing some ad-hoc
  code to convert the prismatic schema to the json schema, which felt
  redundant with the coercer functionality
- The server ended up being more lenient in what it would accept than
  I would like -- e.g., you could give it a json map that contained
  camel-cased keys or kebab-cased keys and it would accept either,
  which led to having production clients using one data format and
  tests using another format. I prefer knowing that both are the same.

So this library is an attempt to solve the problem of transforming
data between different formats in a more first-class way.

The motivating use is to have a schema that describes the
clojure-fluent data format for something, and then specify schema
transformation functions to convert that schema to the variant
format. The library composes the functions in much the same way
that prismatic's coercers do, but the final result is:

- a new schema for the variant format
- two functions for converting objects between the two formats

which I believe addresses the drawbacks listed above.

## Examples

``` clojure
(require '[schema.core :as s]
         '[camel-snake-kebab.core :as csk]
         '[com.gfredericks.schema-bijections :as sb])

(def camelize-keys (sb/transform-keys csk/->camelCase))

(defn jsonify-schema
  [schema]
  (let [{:keys [left left->right right->left]}
        (sb/schema->bijection schema
                              [sb/stringify-uuids
                               sb/stringify-keys
                               camelize-keys])]
    {:json-schema left
     :to-json right->left
     :from-json left->right}))

(def User
  {:id s/Uuid
   :full-name s/Str
   :comments [{:id s/Uuid, :text s/Str}]})

(let [{:keys [json-schema from-json to-json]}
      (jsonify-schema User)]
  (def json->User from-json)
  (def User->json to-json)
  (def UserJson json-schema))

UserJson
=> {#schema.core.RequiredKey{:k "id"}
    #"^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$",

    #schema.core.RequiredKey{:k "fullName"}
    java.lang.String,

    #schema.core.RequiredKey{:k "comments"}
    [{#schema.core.RequiredKey{:k "id"}
      #"^[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}$",

      #schema.core.RequiredKey{:k "text"}
      java.lang.String}]}

(def my-user {:id #uuid "0e90ab35-c550-40ff-ba8a-9e7da774c62c"
              :full-name "Tom Hanks"
              :comments [{:id #uuid "75d450b5-ebc7-43e5-a48c-b11f93e5f7dd"
                          :text "Leave me be."}]})

(User->json my-user)
=> {"comments"
    [{"id" "75d450b5-ebc7-43e5-a48c-b11f93e5f7dd",
      "text" "Leave me be."}],
    "fullName" "Tom Hanks",
    "id" "0e90ab35-c550-40ff-ba8a-9e7da774c62c"}

(= my-user (-> my-user User->json json->User))
=> true
```

## TODOs, caveats, etc.

- Should the conversion functions always do full validation? Should
  that be configurable?
- Integrating this with prismatic/fnhouse is currently a bit awkward.
- Does the schema `1.0.0` change from `either` schemas to conditionals
  make it difficult to do anything generic with them? (i.e., without
  having to figure out how to "translate" each predicate)

## License

Copyright © 2015 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
