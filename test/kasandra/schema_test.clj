(ns kasandra.schema-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [kasandra
             [schema :as k]
             [specs :as specs]]
            [kasandra.test-data :as data
             :refer [table-no-options
                     table-no-ks
                     table+options-no-clustering-key
                     some-table
                     table+clustering-key]]
            [clj-antlr.core :as antlr]))

(def cql-antlr-parser (antlr/parser (slurp (io/resource "kasandra/CqlParser2.g4"))))

(defn first-parsed-table
  [table-str]
  (-> cql-antlr-parser
      (antlr/parse table-str)
      (k/find-cql :createTable)
      first))

(defn first-parsed-view
  [table-str]
  (-> cql-antlr-parser
      (antlr/parse table-str)
      (k/find-cql :createMaterializedView)
      first))


(deftest basic-match-table-tests
  (let [parsed-no-options (first-parsed-table table-no-options)
        {columns ::k/columns
         :as matched} (k/match-create-table parsed-no-options)]
    (is (some? parsed-no-options))
    (is (= (::k/table matched) "foobar"))
    (is (= (::k/keyspace matched) "foobar_ks"))
    (is (= (count columns) 4))          ;3 cols + primary key def
    (is (not (contains? matched ::k/table-options)))
    (is (not (contains? matched ::k/clustring-key)))))

(deftest table-with-options-no-clustering-key
  (let [parsed (first-parsed-table table+options-no-clustering-key)
        matched (k/match-create-table parsed)]
    (is (contains? matched ::k/table-options))
    (is (not (contains? matched ::k/clustering-key)))
    (is (some? matched))))

(deftest basic-match-table-no-keyspace
  (let [parsed (first-parsed-table table-no-ks)
        matched (k/match-create-table parsed)]
    (is (= (::k/table matched) "foobar"))
    (is (not (contains? matched ::k/keyspace)))))

(deftest primary-keys
  (testing "clustering options found"
   (let [parsed (first-parsed-table some-table)
         {table-opts ::k/table-options
          clustering-key ::k/clustering-key} (k/match-create-table parsed)]
     (is parsed)
     (is (= (count table-opts) 2))
     (is (= [["user_id" :asc] ["id" :asc]] clustering-key)))
   (testing "primary key with implied partition/clustering syntax"
     (let [parsed (first-parsed-table data/table-with-primary-key-1)
           create-table-match (k/match-create-table parsed)
           {columns ::k/columns} create-table-match
           primary-key-col (last columns)
           _ (assert primary-key-col)
           primary-key (k/match-primary-key (first (drop 4 primary-key-col)))]
       (is primary-key)))
   (testing "with clustering key"
     (let [parsed (first-parsed-table data/table+clustering-key)
           {columns ::k/columns} (k/match-create-table parsed)
           primary-key-col (last columns)
           primary-key (k/match-primary-key (first (drop 4 primary-key-col)))]
       (is primary-key)))))

(deftest match-column-definition-inline-primary-key
  (let [parsed (first-parsed-table data/table-inline-primary-key)
        {[first-col] ::k/columns} (k/match-create-table parsed)
        [pk-column] (k/match-column-definition first-col)]
    (is (= :pk-column pk-column))))

(deftest create-table-tests
  (let [parsed (-> cql-antlr-parser
                   (antlr/parse some-table)
                   (k/find-cql :createTable)
                   first)
        table (k/create-table parsed)]
    (is (s/valid? :kasandra.schema/create-table table))
    (is (= {::k/partition-key ["org_id" "id"]
            ::k/clustering-key [["user_id" :asc]
                                      ["id" :asc]]}
           (::k/primary-key table)))))

(deftest basic-match-materialized-view-test
  (let [parsed-no-options (first-parsed-view data/materialized-view-1)
        {columns ::k/view-columns
         :as matched} (k/match-create-view parsed-no-options)]
    (is (s/valid? :kasandra.schema/create-materialized-view matched))
    (is (some? parsed-no-options))
    (is (= (::k/view matched) "activity_by_message"))
    (is (= (::k/keyspace matched) "ks_test"))
    (is (= (count columns) 10))
    (is (not (contains? matched ::k/table-options)))
    (is (not (contains? matched ::k/clustring-key)))))

(deftest create-view-tests
  (let [parsed-no-options (first-parsed-view data/materialized-view-1)
        view (k/create-view parsed-no-options)]
    (is (s/valid? :kasandra.schema/create-materialized-view view))))

(deftest regression-broken-pk-extraction
  (let [table-cql "CREATE TABLE foo_bar (foo_id uuid PRIMARY KEY, bar_text text)"
        info (k/schema-info (k/->MatchBasedSchemaInfoProvider)
                                  table-cql)]
    (is (s/valid? :kasandra.schema/create-table (first (:tables info))))))

(deftest table-with-ttl
  (let [table-cql "create table some_random_table ( foo_id uuid, label text, id uuid, user_id uuid, primary key ((foo_id, label),id) ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy', 'tombstone_compaction_interval': '86400'} AND default_time_to_live = 2592000;"
        info (k/schema-info (k/->MatchBasedSchemaInfoProvider)
                                  table-cql)
        ;; parsed (first-parsed-table table-cql)
        table (first (:tables info))]
    (is (contains? (set (map :kasandra.schema/option-name (:kasandra.schema/table-options table)))
                   "default_time_to_live"))))

(deftest regression-incorrect-clustring-key
  (let [table-cql "create table foo_bar ( foo_id uuid, label text, id uuid, primary key ((foo_id,label),id) ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy', 'tombstone_compaction_interval': '86400'} AND default_time_to_live = 2592000"
        {[table ]:tables} (k/schema-info (k/->MatchBasedSchemaInfoProvider) table-cql)]
    (is (s/valid? :kasandra.schema/create-table table))))

