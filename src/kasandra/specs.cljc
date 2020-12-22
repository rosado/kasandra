(ns kasandra.specs
  (:require [clojure.spec.alpha :as s]))

(defn table? [m] (= (type m) :kasandra.schema/table))

(s/def ::table table?)

(defn namespaced-keyword? [kw] (not (nil? (namespace kw))))

(s/def :kasandra.schema/type (s/and keyword? namespaced-keyword?))

(s/def :kasandra.schema/view string?)

(s/def :kasandra.schema/keyspace string?)

(s/def :kasandra.schema/base-table string?)

(s/def :kasandra.schema/column-def
  (s/tuple string? (s/keys :req-un [:kasandra.schema.column/type]
                           :opt-un [:kasandra.schema.column/mapping
                                    :kasandra.schema.column/of])))

(s/def :kasandra.schema/columns (s/coll-of :kasandra.schema/column-def :kind vector?))

(s/def :kasandra.schema/primary-key
  (s/keys :req [:kasandra.schema/partition-key]
          :opt [:kasandra.schema/clustering-key]))

(s/def :kasandra.schema/clustering-key-element
  (s/tuple string? #{:asc :desc}))

(s/def :kasandra.schema/clustering-key (s/coll-of :kasandra.schema/clustering-key-element
                                               :kind vector?))


(s/def :kasandra.schema/partition-key-element string?)

(s/def :kasandra.schema/partition-key (s/coll-of :kasandra.schema/partition-key-element
                                              :kind vector?))

(s/def :kasandra.schema/clustering-order
  (s/keys :req [:kasandra.schema/clustering-key]))

;;; TODO: no specs for option-type, option-value, option-hash, option-name
(s/def :kasandra.schema/table-option (s/keys :req [:kasandra.schema/option-type
                                                :kasandra.schema/option-name]
                                          :opt [:kasandra.schema/option-hash
                                                :kasandra.schema/option-value]))

(s/def :kasandra.schema/table-options (s/coll-of :kasandra.schema/table-option
                                              :kind vector?))

(s/def :kasandra.schema/create-table (s/keys :req [:kasandra.schema/type
                                                :kasandra.schema/table
                                                :kasandra.schema/columns
                                                :kasandra.schema/primary-key]
                                          :opt [:kasandra.schema/keyspace
                                                :kasandra.schema/table-options]))

(s/def :kasandra.schema/view-columns (s/coll-of string? :kind vector?))

(s/def :kasandra.schema/create-materialized-view
  (s/keys :req [:kasandra.schema/type
                :kasandra.schema/view
                :kasandra.schema/base-table
                :kasandra.schema/view-columns
                :kasandra.schema/primary-key]
          :opt [:kasandra.schema/keyspace
                :kasandra.schema/view-options
                ;;:kasandra.schema/clustering-order
                ]))




