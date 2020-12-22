(ns kasandra.schema
  "Utilities for creating/navigating an in-memory represenatation of a Cassandra keyspace.
  
  Notes: expect functions with `match-X` prefix to mostly do matching (via core.match).
  Functions without the `match-X` prefix should return objects ready for use.

  The CQL parser uses a slightly modified ANTLR grammar file available from 

  https://github.com/antlr/grammars-v4/tree/master/cql3"
  (:require [clj-antlr.core :as antlr]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import (clojure.lang ExceptionInfo)))

(defn- all-cql
  "Takes output of `clj-antlr.core/parse` and collects all CQL forms into a seq."
  [parsed]
  (let [cqls (-> parsed second next)]
    (for [cql cqls :when (= :cql (first cql))]
      (second cql))))

(defn find-cql
  "Takes output of `clj-antlr.core/parse` and collects all CLQ forms starting with given keyword.
  Returns a seq.

  - `kw` keyword, e.g. `:createTable`"
  [parsed kw]
  (let [cqls (-> parsed second next)]
    (for [cql cqls :when (and (= :cql (first cql))
                              (= kw (-> cql second first)))]
      (second cql))))

(defn- cons? [form] (instance? clojure.lang.Cons form))

(defn- vectorize
  [form]
  (mapv (fn [inner]
          (if (cons? inner)
            (vec inner)
            inner))
        form))

(defn deep-vectorize
  "clj-antlr uses cons list, we want to turn them into vectors
  because core.match's syntax for matching vectors is less verbose."
  [form]
  (vectorize (walk/prewalk (fn [v]
                              (if (cons? v)
                                (vec v)
                                v))
                            form)))

(defn syntax-comma?
  [form]
  (and (not (keyword? form))
       (seq form)
       (= :syntaxComma (first form))))

(defn- remove-syntax-commas [fs] (remove syntax-comma? fs))

(defn- -simplify-key-column
  "Returns column name."
  [col-match]
  (match [col-match]
         [[:partitionKey [:column column-name]]] column-name

         [[:clusteringKey [:column column-name]]] column-name

         [[:column column-name]] column-name))

(defn make-primary-key
  [partition-key clustering-key]
  (cond-> {::partition-key partition-key}
    ;; default to :asc order, will most likely be overriden by clutering-order from options
    (some? clustering-key) (assoc ::clustering-key (mapv #(vector % :asc) clustering-key))))

(defn match-primary-key
  [form]
  (let [simplify-key-cols (fn [key-match-list]
                            (->> key-match-list
                                 remove-syntax-commas
                                 (mapv -simplify-key-column)))]
    (try
      (match [form]
             [[:primaryKeyDefinition    ;DONE: matched
               [:compositeKey
                [:syntaxBracketLr _ ]
                [:partitionKeyList & partition-key-list]
                [:syntaxBracketRr _] ] ]]
             (make-primary-key (simplify-key-cols partition-key-list) nil)

             ;; --------------------------------
             [[:primaryKeyDefinition    ;TODO: potentially not needed
               [:compositeKey _ [:partitionKeyList & partition-key-list] _ ]]]
             (make-primary-key (simplify-key-cols partition-key-list) nil)

             [[[:primaryKeyDefinition
                [:compositeKey _ [:partitionKeyList & partition-key-list]
                 _ ;;[:syntaxBracketRr ")"]
                 ]] _ ]
              ]
             (make-primary-key (simplify-key-cols partition-key-list) nil)

             ;; --------------------------------
             [[[:primaryKeyDefinition   ;TODO: potentially not needed
                [:compositeKey
                 _
                 [:partitionKeyList & partition-key-list]
                 _
                 [:syntaxComma ","]
                 [:clusteringKeyList & clustering-key-list]]]
               [:syntaxBracketRr ")"]]]
             (make-primary-key (simplify-key-cols partition-key-list)
                               (simplify-key-cols clustering-key-list))

             ;; --------------------------------
             [[:primaryKeyDefinition ;DONE: matched
               [:compoundKey
                [:partitionKey & partition-key]
                _
                [:clusteringKeyList & clustering-key-list]]]]
             (make-primary-key (simplify-key-cols partition-key)
                               (simplify-key-cols clustering-key-list))

             [[[:primaryKeyDefinition   ;TODO: potentially not needed
                [:compoundKey
                 [:partitionKey & partition-key]
                 _
                 [:clusteringKeyList & clustering-key-list]]]
               [:syntaxBracketRr _]]]
             (make-primary-key (simplify-key-cols partition-key)
                               (simplify-key-cols clustering-key-list))

             ;; --------------------------------
             [[:primaryKeyDefinition    ;DONE: matched
               [:compositeKey
                [:syntaxBracketLr _]
                [:partitionKeyList
                 & partition-key-list ;; elems like [:partitionKey [:column "org_id"]] [:syntaxComma ","]
                 ]
                [:syntaxBracketRr _]
                [:syntaxComma _]
                [:clusteringKeyList
                 & clustering-key-list ;; elems like [:clusteringKey [:column "name"]]
                 ]]]]
             (make-primary-key (simplify-key-cols partition-key-list)
                               (simplify-key-cols clustering-key-list)))
      (catch Exception ex
        (throw (ex-info "Primary key match failure."
                        {:type ::primary-key-match-failure
                         :form form}
                        ex))))))

(defn- match-data-type
  "Returns a map {:type ... } (+ metadata tagged with :type).

Types are matched as maps, with metadata tagged with one of: 
  - :kasandra.schema.column.type/value
  - :kasandra.schema.column.type/collection
  - :kasandra.schema.column.type/map"
  [form]
  (match [(vectorize form)]
         [[:dataType [:dataTypeName type-name]]]
         ^{:type :kasandra.schema.column.type/value}
         {:type type-name}

         [[:dataType [:dataTypeName type-name] [:dataTypeDefinition
                                                _ ((:dataTypeName of-type) :seq) _ ]]]

         ^{:type :kasandra.schema.column.type/collection}
         {:type type-name
          :of of-type}

         [[:dataType
           [:dataTypeName type-name]
           [:dataTypeDefinition
            _
            ((:dataTypeName key-type) :seq)
            _
            ((:dataTypeName value-type) :seq)
            _ ]]]
         ^{:type :kasandra.schema.column.type/map}
         {:type type-name
          :mapping {key-type value-type}}))

(defn match-column-definition
  "returns  a tuple of [(:column | :pk | :pk-column) payload]"
  [form]
  {:post [(not (nil? %))]}
  (match [form]
         [[:columnDefinition [:column column-name] data-type]]
         [:column [column-name (match-data-type data-type)]]

         [[:columnDefinition [:column column-name] data-type [:primaryKeyColumn _ _]]]
         [:pk-column [column-name (match-data-type data-type)]]

         [[:primaryKeyElement _ _ _ primary-key-elems & _]]
         [:pk (match-primary-key primary-key-elems)]))

(defn- keep-matching
  [matcher kw forms]
  (loop [form (first forms) forms (next forms) results []]
    (cond
      (nil? form)
      results
      
      (and (vector? form)
           (= (first form) kw))
      (let [m (try
                (matcher form)
                (catch Exception ex
                  (throw (ex-info (str "Failed to match form:" (pr-str form) ", " (.getMessage ex))
                                  {:type ::match
                                   :form form}))))]
        (recur (first forms) (next forms) (conj results m)))

      :else
      (recur (first forms) (next forms) results))))

(defn- strip-quotes
  [s]
  (subs s 1 (dec (count s))))

(defn match-option-hash-item
  [form]
  {:pre [(= (first form) :optionHashItem)]}
  (match [form]
         [[:optionHashItem
           [:optionHashKey [:stringLiteral item-key]]
           _
           [:optionHashValue [:stringLiteral item-value]]]]
         {(strip-quotes item-key) (strip-quotes item-value)}))

(defn match-create-table-option-item
  [form]
  {:pre [(= :tableOptionItem (first form))]}
  (match [form]
         [[:tableOptionItem
           [:tableOptionName option-name]
           _
           [:tableOptionValue option-value]]]
         {::option-type ::value
          ::option-name option-name
          ::option-value option-value}

         [[:tableOptionItem
           [:tableOptionName option-name]
           _
           [:optionHash & option-hash]]]
         {::option-type ::hash
          ::option-name option-name
          ::option-hash (apply merge
                               (keep-matching match-option-hash-item
                                              :optionHashItem
                                              option-hash))}))

(defn match-create-table-options
  [form]
  (let [tail-opts (first (filter (fn [v]
                                   (and (vector? v)
                                        (= (first v) :tableOptions)))
                                 form))]
    (vec
     (concat (keep-matching match-create-table-option-item :tableOptionItem form)
             (keep-matching match-create-table-option-item :tableOptionItem tail-opts)))))

(defn match-create-table-clustering-key-column
  [form]
  (match [(vec form)]
         [[[:column column-name]
           [:orderDirection [:kwAsc _]]]]
         [column-name :asc]

         [[[:column column-name]
           [:orderDirection [:kwDesc _]]]]
         [column-name :desc]))

(defn match-create-table-clustering-key
  "Returns a vector of tuples like [\"col-name\" :asc]"
  [form]
  (let [sanitized (vec (remove syntax-comma? form))]
    (loop [current (take 2 sanitized) remaining (drop 2 sanitized) result []]
      (if (empty? current)
        result
        (recur (take 2 remaining)
               (drop 2 remaining)
               (conj result (match-create-table-clustering-key-column current)))))))

(defn match-create-table-with
  "Returns a map of shape:
  {::table-options ...
   ::clustering-key [[string kw]*]}"
  [form]
  {:pre [(= :withElement (first form))]}
  (let [matched (match [form]
                       [[:withElement
                         [:kwWith _]
                         [:clusteringOrder
                          [:kwClustering _] [:kwOrder _] [:kwBy _]
                          [:syntaxBracketLr _]
                          [:clusteringOrderKey & clustering-key]
                          [:syntaxBracketRr _]]
                         [:tableOptions & table-options]]]
                       (with-meta
                         (cond-> {::clustering-key (match-create-table-clustering-key clustering-key)}

                           (not (empty? table-options))
                           (assoc ::table-options (match-create-table-options table-options)))
                         
                         {:type ::create-table-with})
                       
                       [[:withElement [:kwWith _] & table-options]] 
                       (with-meta
                         (cond-> {}
                           (not (empty? table-options))
                           (assoc ::table-options (match-create-table-options table-options)))
                         
                         {:type ::create-table-with}))]
    matched))

(defn match-create-table
  "Returns a map of shape:
  {::table ...
   ::keyspace ...
   ::columns []}

  merged with keys returned by `match-create-table-with`"
  [form]
  {:pre [(= :createTable (first form))]}
  (let [form* (deep-vectorize form)
        make (fn [ks-name table-name column-defs with-items]
               (with-meta
                 (cond-> {::type ::table
                          ::table table-name
                          ::columns (vec (remove syntax-comma? column-defs))}

                   (some? ks-name)
                   (assoc ::keyspace ks-name)

                   (not (empty? with-items))
                   (merge (match-create-table-with (first with-items))))
                 {:type ::table}))]
    (match [form*]
           [[:createTable _ _ [:keyspace ks-name] _ [:table table-name] _
             [:columnDefinitionList & column-defs]
             _             ;right bracket
             & with-items
             ]]
           (make ks-name table-name column-defs with-items)

           [[:createTable _ _  [:table table-name] _
             [:columnDefinitionList & column-defs]
             _             ;right bracket
             & with-items
             ]]
           (make nil table-name column-defs with-items))))

(defn- find-primary-key-in-columns
  [columns]
  (reduce (fn [[pk cols] col-match]
            (case (first col-match)
              :pk [(second col-match) cols]
              :pk-column [(make-primary-key [(first (second col-match))] nil)
                          (conj cols (second col-match))]
              :column [pk (conj cols (peek col-match))]))
          [nil []]
          columns))

(defn create-table
  "Returns"
  [form]
  (try
    (let [matched (match-create-table form)
          matched-cols (map match-column-definition (matched ::columns))
          [primary-key cols] (find-primary-key-in-columns matched-cols)]
      (cond-> matched
        (some? primary-key) (assoc ::primary-key primary-key
                                   ::columns cols)
        (some? (::clustering-key matched))
        (assoc-in [::primary-key ::clustering-key] (::clustering-key matched))))
    (catch ExceptionInfo ex
      (let [typ (:type (ex-data ex))]
        (case typ
          ::primary-key-match-failure (throw ex)
          (throw (ex-info "'create-table' form couldn't be created."
                          {:type ::create-table-failure
                           :form form}
                          ex)))))))

(defn match-view-clustering-order
  [form]
  (match [form]
         [[:clusteringOrder
           [:kwClustering _] [:kwOrder _] [:kwBy _]
           [:syntaxBracketLr _]
           [:clusteringOrderKey & clustering-key]
           [:syntaxBracketRr _]]]
         {::clustering-key (match-create-table-clustering-key clustering-key)}))

(defn match-create-view
  [form]
  {:pre [(= :createMaterializedView (first form))]}
  (let [form* (deep-vectorize form)
        matched (match [form*]
                       [[:createMaterializedView
                         _ _ _ [:keyspace ks-name] _ [:materializedView view-name] 
                         _ _            ; AS SELECT
                         [:columnList & column-list]
                         _ _ _               ;FROM $KEYSPACE .
                         [:table from-table]
                         [:materializedViewWhere _
                          [:columnNotNullList & not-null-cols]]
                         _ _ _
                         primary-key-form;;[:primaryKeyDefinition primary-key-def]
                         _ _
                         [:materializedViewOptions
                          clustering-order-form
                          view-options-form]]]
                       (with-meta
                         (let [view-options (match-create-table-options view-options-form)
                               clustering-order (match-view-clustering-order clustering-order-form)]
                           (cond->
                               {::type ::materialized-view
                                ::view view-name
                                ::base-table from-table
                                ::view-columns (->> column-list
                                                    (remove syntax-comma?)
                                                    (mapv second))
                                ::primary-key (merge (match-primary-key primary-key-form)
                                                     clustering-order)
                                ::clustering-order clustering-order}
                             (some? view-options) (assoc ::view-options view-options)
                             (some? ks-name) (assoc ::keyspace ks-name)))
                         {:type ::materialized-view}))]
    matched))

(defn create-view
  [form]
  (match-create-view form))

(defmulti -column-names
  "Returns a vector of column names (strings)"
  type)

(defmethod -column-names ::table [definition]
  (mapv first (::columns definition)))

(defmethod -column-names ::materialized-view [definition]
  (::view-columns definition))

(defn column-names
  "Returns a vector of column names (strings)"
  [definition]
  (-column-names definition))

(defn primary-key-column-names
  "Returns a vector of column names comprising the primary key."
  [table-or-view]
  (let [{partition-key ::partition-key
         clustering-key ::clustering-key} (::primary-key table-or-view)]
    (into partition-key (map first clustering-key))))

(defprotocol SchemaParser
  "Parses DB schema and outputs something `TableInfoProvider` and
  `ViewInfoProvider` can use."
  (-parse [self input]))

(defprotocol TableInfoProvider
  "Extracts table definition from parser output."
  (-tables [this parsed] "Returns a collection of table definitions."))

(defprotocol ViewInfoProvider
  "Extracts materialized view definitions from parser output"
  (-views [this parsed] "Returns a collection of view definitions"))

(defprotocol FormFinder
  "Finds occurences of tables or views in parser output."
  (-find-tables [_ opts] "Returns a collection of `:kasandra.schema/table`")
  (-find-views [_ opts] "Returns a collection of `:kasandra.schema/materialized-view`"))

(defrecord CljAntlrOutput [output]
  FormFinder
  (-find-tables [_ opts]
    (find-cql output :createTable))
  (-find-views [_ opts]
    (find-cql output :createMaterializedView)))

(defrecord MatchBasedSchemaInfoProvider []
  SchemaParser
  (-parse [this cql]
    (let [parser (antlr/parser (slurp (io/resource "kasandra/CqlParser2.g4")))]
      (->CljAntlrOutput (antlr/parse parser cql))))
  TableInfoProvider
  (-tables [_ parsed]
    (mapv create-table (-find-tables parsed {})))
  ViewInfoProvider
  (-views [_ parsed]
    (mapv create-view (-find-views parsed {}))))

(defn tables
  "Returns a collection of table definitions."
  [provider parsed]
  (-tables provider parsed))

(defn materialized-views
  "Returns a collection of materialized view defintions."
  [provider parsed]
  (-views provider parsed))

;;; Utilities

(defn schema-index
  "Creates an index for easy look up of tables/views by name"
  [schema-info]
  (let [{views :views tables :tables} schema-info]
    (as-> {} index
      (reduce (fn [m [idx table]]
                (assoc m (:kasandra.schema/table table) [:tables idx]))
              index
              (map-indexed #(vector %1 %2) tables))
      (reduce (fn [m [idx view]]
                (assoc m (:kasandra.schema/view view) [:views idx]))
              index
              (map-indexed #(vector %1 %2) views)))))

(defn add-index
  "Adds :index to schema-info"
  [schema-info]
  (assoc schema-info :index (schema-index schema-info)))

(defn lookup-by-name
  "Given the schema info (which should include the index)
  get the table or view with that name or nil."
  [schema-info name]
  {:pre [(contains? schema-info :index)]}
  (when-let [location (get-in schema-info [:index name])]
    (get-in schema-info location)))

(defn search-schema-info
  "Returns a seq of tuples [name location] where location
  is a path to the item in passed schema-info."
  [schema-info regex]
  {:pre [(contains? schema-info :index)]}
  (keep (fn [kv]
          (when (re-find regex (key kv))
            kv))
        (:index schema-info)))

(defn schema-info
  "Returns a map containing :tables and :views.
  Supported options:
  - :index - set to :include and result map will contain :index
            entry with a map of name -> path that can be used
            to lookup the item in the schema info."
  ([provider cql]
   (schema-info provider cql {}))
  ([provider cql options]
   (let [parsed (-parse provider cql)
         info (with-meta
                {:tables (tables provider parsed)
                 :views (materialized-views provider parsed)}
                {:type :kasandra.schema/schema-info})]
     (cond-> info
       (= (:index options) :include)
       (assoc :index (schema-index info))))))


;;; TODO: introduce :kasandra.schema.table/name & :kasandra.schema.view/name ?
;;; TODO: introduce :kasandra.schema.table/columns :kasandra.schema.view/columns ?

(comment


  (def result (schema-info (->MatchBasedSchemaInfoProvider)
                           (slurp "yapdev-keyspace.cql")))
  )
