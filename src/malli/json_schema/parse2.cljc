(ns malli.json-schema.parse2
  (:require [clojure.set :as set]))

(declare schema->malli)

(def annotations #{:title :description :default :examples :example})

(defn annotations->properties [js-schema]
  (-> js-schema
      (select-keys annotations)
      (set/rename-keys {:examples    :json-schema/examples
                        :example     :json-schema/example
                        :title       :json-schema/title
                        :description :json-schema/description
                        :default     :json-schema/default})))

;; Utility Functions
#_(defn- map-values
    ([-fn] (map (fn [[k v]] [k (-fn v)])))
    ([-fn coll] (sequence (map-values -fn) coll)))

;; Parsing
(defmulti type->malli :type)

(defn $ref [js-schema]
  [:schema [:ref (:$ref js-schema)]])

#_(mu/update-properties
   ...
   merge
   (annotations->properties js-schema))

(defn properties->malli [required [k v]]
  (cond-> [k]
    (nil? (required k)) (conj {:optional true})
    true (conj (schema->malli v))))

(defn- prop-size [pred?] (fn [-map] (pred? (count (keys -map)))))
(defn- min-properties [-min] (prop-size (partial <= -min)))
(defn- max-properties [-max] (prop-size (partial >= -max)))

(defn with-min-max-properties-size [malli v]
  (let [predicates [(some->> v
                             (:minProperties)
                             (min-properties)
                             (conj [:fn]))
                    (some->> v
                             (:maxProperties)
                             (max-properties)
                             (conj [:fn]))]]
    (cond->> malli
      (some some? predicates)
      (conj (into [:and]
                  (filter some?)
                  predicates)))))

(defn object->malli [{:keys [additionalProperties] :as v}]
  (let [required (into #{}
                       ;; TODO Should use the same fn as $ref
                       (map keyword)
                       (:required v))
        closed? (false? additionalProperties)]
    (-> (if (:type additionalProperties)
          (let [va (schema->malli additionalProperties)] [:map-of va va])
          [:map])
        (cond-> closed? (conj {:closed :true}))
        (into
         (map (partial properties->malli required))
         (:properties v))
        (with-min-max-properties-size v))))

(defn schema->malli [js-schema]
  (cond
    (boolean? js-schema) :any
    (contains? js-schema :type) (type->malli js-schema)

    (contains? js-schema :properties) [:or (object->malli js-schema) [:not 'map?]]

    (contains? js-schema :enum) (into [:enum]
                                      (:enum js-schema))

    (contains? js-schema :const) [:= (:const js-schema)]

    ;; Aggregates
    (contains? js-schema :oneOf) (into
                                   ;; TODO Figure out how to make it exclusively select o schema
                                   ;; how about `m/multi`?
                                  [:or]
                                  (map schema->malli)
                                  (:oneOf js-schema))

    (contains? js-schema :anyOf) (into
                                  [:or]
                                  (map schema->malli)
                                  (:anyOf js-schema))

    (contains? js-schema :allOf) (into
                                  [:and]
                                  (map schema->malli)
                                  (:allOf js-schema))

    (contains? js-schema :not) [:not (schema->malli (:not js-schema))]

    (contains? js-schema :$ref) ($ref js-schema)

    (empty? js-schema) :any

    :else :any #_(throw (ex-info "Not supported" {:json-schema js-schema
                                                  :reason      ::schema-type}))))

(defmethod type->malli "string" [{:keys [pattern minLength maxLength enum format]}]
  ;; `format` metadata is deliberately not considered.
  ;; String enums are stricter, so they're also implemented here.
  (cond
    pattern [:re pattern]
    enum (into [:enum] enum)
    (= format "uuid") :uuid
    :else (let [attrs (cond-> nil
                        minLength (assoc :min minLength)
                        maxLength (assoc :max maxLength))]
            (if attrs
              [:string attrs]
              :string))))

(defn- number->malli [{:keys [minimum maximum exclusiveMinimum exclusiveMaximum
                              _multipleOf enum type]
                       :as _schema}]
  (let [integer (= type "integer")
        implicit-double (or minimum maximum integer enum
                            (number? exclusiveMaximum) (number? exclusiveMinimum))
        maximum (if (number? exclusiveMaximum) exclusiveMaximum maximum)
        minimum (if (number? exclusiveMinimum) exclusiveMinimum minimum)]
    (cond-> (if integer [:int] [])
      (or minimum maximum) identity
      enum (into [(into [:enum] enum)])
      maximum (into [[(if exclusiveMaximum :< :<=) maximum]])
      minimum (into [[(if exclusiveMinimum :> :>=) minimum]])
      (not implicit-double) (into [[:double]]))))

(defmethod type->malli "integer" [p]
  ;; TODO Implement multipleOf support
  (let [ranges-logic (number->malli p)]
    (if (> (count ranges-logic) 1)
      (into [:and] ranges-logic)
      (first ranges-logic))))

(defmethod type->malli "number" [{:keys [_exclusiveMinimum _exclusiveMaximum _minimum _maximum] :as p}]
  (let [ranges-logic (number->malli p)]
    (if (> (count ranges-logic) 1)
      (into [:and] ranges-logic)
      (first ranges-logic))))

(defmethod type->malli "boolean" [_p] boolean?)
(defmethod type->malli "null" [_p] :nil)
(defmethod type->malli "object" [p] (object->malli p))
(defmethod type->malli "array" [p] (let [items (:items p)]
                                     (cond
                                       (vector? items) (into [:tuple]
                                                             (map schema->malli)
                                                             items)
                                       (:uniqueItems p) [:set (schema->malli items)]
                                       (map? items) [:vector (schema->malli items)]
                                       :else (throw (ex-info "Not Supported" {:json-schema p
                                                                              :reason ::array-items})))))

(defmethod type->malli "file" [_p]
  [:map {:json-schema {:type "file"}} [:file :any]])

(defmethod type->malli :default [{:keys [type] :as p}]
  (cond
    (vector? type) (into [:or] (map #(type->malli {:type %}) type))
    (and type (= 1 (count (keys p)))) {:json-schema/type type}
    :else
    (throw (ex-info "Not Supported" {:json-schema p
                                     :reason ::unparseable-type}))))

(defn json-schema-document->malli [obj]
  [:schema {:registry (-> {"#" (schema->malli obj)}
                          (into (map (fn [[k v]]
                                       [(str "#/definitons/" (name k)) (schema->malli v)]))
                                (:definitions obj))
                          (into (map-indexed (fn [idx x]
                                               [(str "#/items/" idx) (schema->malli x)]))
                                (:items obj)))}
   "#"])
