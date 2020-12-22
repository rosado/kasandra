(ns kasandra.rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clara.rules :as clara]
            [clara.tools.inspect :as inspect]
            [kasandra.schema :as k]
            [kasandra.specs]
            [kasandra
             [rules :as rules]]
            [kasandra.rules
             [intents :as intents]
             [effects :as fx]
             [relations :as relations]]))


(def table-a1
  ^{:type ::k/table}
  #::k{:type :kasandra.schema/table,
       :table "a1",
       :columns
       [["org_id" {:type "uuid"}]
        ["conversation_id" {:type "uuid"}]
        ["option_ids" {:type "list", :of "uuid"}]
        ["poll_time" {:type "timestamp"}]
        ["poll_type" {:type "text"}]],
       :keyspace "test_keyspace",
       :table-options
       [#::k{:option-type :kasandra.schema/value,
             :option-name "bloom_filter_fp_chance",
             :option-value [:floatLiteral "0.01"]}],
       :primary-key
       {::k/partition-key ["org_id" "conversation_id"]}})

(assert (= ::k/table (type table-a1)))
(assert (s/valid? ::k/create-table table-a1))

(def view-for-table-a1
  ^{:type ::k/materialized-view}
  #::k{:type ::k/create-materialized-view
       :view "a1_view"
       :base-table "a1"
       :view-columns ["org_id" "conversation_id" "option_ids" "poll_time" "poll_type"]
       :primary-key {::k/partition-key ["org_id" "conversation_id"]
                     ::k/clustering-key [["poll_type" :asc]]}})

(def table-a1-secondary
  ^{:type ::k/table}
  #::k{:type :kasandra.schema/table,
       :table "a1_by_conversation",
       :columns
       [["org_id" {:type "uuid"}]
        ["conversation_id" {:type "uuid"}]
        ["option_ids" {:type "list", :of "uuid"}]
        ["poll_time" {:type "timestamp"}]
        ["poll_type" {:type "text"}]],
       :keyspace "test_keyspace",
       :table-options
       [#::k{:option-type :kasandra.schema/value,
             :option-name "bloom_filter_fp_chance",
             :option-value [:floatLiteral "0.01"]}],
       :primary-key
       {::k/partition-key ["conversation_id"]}})


(def table-a1-lookup
  ^{:type ::k/table}
  #::k{:type :kasandra.schema/table,
       :table "a1_lookup",
       :columns
       [["org_id" {:type "uuid"}]
        ["conversation_id" {:type "uuid"}]
        ["poll_type" {:type "text"}]],
       :keyspace "test_keyspace",
       :table-options
       [#::k{:option-type :kasandra.schema/value,
             :option-name "bloom_filter_fp_chance",
             :option-value [:floatLiteral "0.01"]}],
       :primary-key
       {::k/partition-key ["poll_type"]}})

(assert (= ::k/materialized-view (type view-for-table-a1)))
(assert (s/valid? ::k/create-materialized-view view-for-table-a1))

(deftest basic-table-operations
  (let [session (rules/make-session)]
    (testing "Creating a table produces effects"
      (let [session (-> session
                        (rules/insert (intents/->CreateTableIntent table-a1))
                        (rules/fire-rules))
            [{effect :?effect} :as fx] (clara/query session rules/effects)]
        (is (= 1 (count fx)))
        (is (= :kasandra.schema/table (type (:definition effect))))))
    
    (testing "Droping a table with a view, also drops the view."
      (let [session (-> session
                        (rules/insert table-a1 view-for-table-a1)
                        (rules/insert (intents/->DropTableIntent "a1"))
                        (rules/fire-rules))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (empty? problems))
        (is (= 2 (count fx)))))))

(deftest secondary-table-operations
  (let [prepare-session (fn [relation]
                          (-> (rules/make-session)
                              (rules/insert table-a1 table-a1-secondary)
                              (rules/insert relation)
                              (rules/insert (intents/->DropTableIntent (::k/table table-a1-secondary)))
                              (rules/fire-rules)))]
    (testing "dropping table via SecondaryTableRelation"
      (let [session (prepare-session (relations/->SecondaryTableRelation (::k/table table-a1)
                                                                         (::k/table table-a1-secondary)))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (empty? problems))
        (is (= #{(fx/->DropTable (::k/table table-a1-secondary))}
               fx))))
    (testing "dropping table via LookupTableRelation"
      (let [session (prepare-session (relations/->LookupTableRelation (::k/table table-a1)
                                                                      (::k/table table-a1-secondary)))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (empty? problems))
        (is (= #{(fx/->DropTable (::k/table table-a1-secondary))}
               fx))))
    (testing "dropping table via UniqueKeyTableRelation"
      (let [session (prepare-session (relations/->UniqueKeyTableRelation (::k/table table-a1)
                                                                         (::k/table table-a1-secondary)))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (empty? problems))
        (is (= #{(fx/->DropTable (::k/table table-a1-secondary))}
               fx))))))

(deftest add-columns
  (let [session (as-> (rules/make-session) sess
                  (rules/insert sess table-a1))
        intent (intents/->AddTableColumnsIntent "a1" [])]
    (testing "adding a new column produces effects"
      (let [session (-> session
                        (rules/insert (assoc intent :columns [{:name "foobar" :type {:type "text"}}]))
                        (rules/fire-rules))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (not (empty? fx)))
        (is (empty? problems))
        (is (contains? (set (map :name (:columns (first fx)))) "foobar"))))
    (testing "adding column with clashing name does not produce any effects"
      (let [session (-> session
                        (rules/insert (assoc intent :columns [{:name "org_id" :type {:type "text"}}]))
                        (rules/fire-rules))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (not (empty? problems)))
        (is (empty? fx))))))

(deftest re-creating-views-full         ;TODO: this is identical with `re-creating-views`
  (let [session (as-> (rules/make-session) sess
                  (rules/insert sess table-a1 view-for-table-a1)
                  (rules/insert sess (relations/->SecondaryTableRelation "a1" "a1_view"))
                  (rules/insert sess (intents/->AddTableColumnsIntent "a1" [{:name "foobar"
                                                                             :type {:type "text"}}]))
                  (rules/fire-rules sess))
        create-view-fx (clara/query session rules/create-views)
        drop-view-fx (clara/query session rules/drop-views)
        fx (clara/query session rules/effects)
        {{{view-columns ::k/view-columns} :definition} :?create-view} (first create-view-fx)]
    (is (not (empty? drop-view-fx)))
    (is (not (empty? create-view-fx)))
    (is (not (empty? fx)))
    (is (= (conj (::k/view-columns view-for-table-a1) "foobar")
           view-columns))))

(deftest secondary-table-updates
  (let [session! (fn [](-> (rules/make-session)
                           (rules/insert table-a1 table-a1-secondary)
                           (rules/insert (relations/->SecondaryTableRelation
                                          (::k/table table-a1)
                                          (::k/table table-a1-secondary)))))]
    (testing "add columns to secondary table when adding cols to primary"
      (let [session (-> (session!)
                        (rules/insert (intents/->AddTableColumnsIntent "a1" [{:name "foobar"
                                                                              :type "text"}]))
                        (rules/fire-rules))
            fx (clara/query session rules/effects)]
        (is (= 2 (count (distinct fx))))))
    (testing "remove columns from secondary when droping cols from primary"
      (let [column {:name "poll_time" :type "timestamp"}
            session (-> (session!)
                        (rules/insert (intents/->RemoveTableColumnsIntent "a1" [column]))
                        (rules/fire-rules))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (= #{(fx/->DropTableColumn (::k/table table-a1) column)
                 (fx/map->DropTableColumn {:table (::k/table table-a1-secondary)
                                           :column column
                                           ::fx/order rules/order-after})}
               fx))))
    (testing "remove non-existing columns results in a Problem"
      (let [column {:name "barf_col" :type "timestamp"}
            session (-> (session!)
                        (rules/insert (intents/->RemoveTableColumnsIntent "a1" [column]))
                        (rules/fire-rules))
            {fx :effects problems :problems} (rules/summarize session)]
        (is (= #{(rules/->ColumnsNotPresent "a1" #{column})}
               problems))))))

(deftest drop-column-cascade-to-view
  (let [prepare-drop-cols (fn prepare-drop-cols [cols]
                            (-> (rules/make-session)
                                (rules/insert table-a1 view-for-table-a1)
                                (rules/insert (relations/->SecondaryTableRelation
                                               (::k/table table-a1)
                                               (::k/view view-for-table-a1)))
                                (rules/insert (intents/->RemoveTableColumnsIntent (::k/table table-a1) cols))
                                (rules/fire-rules)))]
    (testing "if there is a drop view effect for the secondary table (view)"
      (let [session (prepare-drop-cols ["poll_time"])
            {fx :effects problems :problems} (rules/summarize session)]
        (is (empty? problems))
        (is (= #{(fx/map->DropView {:view (::k/view view-for-table-a1)
                                    ::fx/order rules/order-before})
                 (fx/map->CreateView {:definition (update view-for-table-a1 ::k/view-columns
                                                          (fn [cols] (remove #(= "poll_time" %) cols)))
                                      ::fx/order rules/order-after})
                 (fx/->DropTableColumn (::k/table table-a1) "poll_time")}
               fx))))
    (testing "trying to drop column in view's primary key is a problem"
      (let [session (prepare-drop-cols ["poll_type"])
            {problems :problems} (rules/summarize session)]
        (is (not (empty? problems)))))))

(deftest drop-column-problems-in-derived-columns
  (let [prepare (fn [relation]
                  (-> (rules/make-session)
                      (rules/insert table-a1 table-a1-lookup)
                      (rules/insert (relation (::k/table table-a1) (::k/table table-a1-lookup)))
                      (rules/insert (intents/->RemoveTableColumnsIntent (::k/table table-a1)
                                                                        #{"poll_type"}))
                      (rules/fire-rules)))]
    (testing "lookup table relation"
      (let [{fx :effects problems :problems} (-> relations/->LookupTableRelation
                                                 (prepare)
                                                 (rules/summarize))]
        (is (not (empty? problems)))))
    (testing "lookup table relation"
      (let [{fx :effects problems :problems} (-> relations/->UniqueKeyTableRelation
                                                 (prepare)
                                                 (rules/summarize))]
        (is (not (empty? problems)))))))

(comment 
  ;;(inspect/explain-activations session)
  ;;(exp/expound ::k/create-materialized-view view-for-table-a1)
  
  (kasandra.dev/tables kasandra.dev/db-schema)

  (kasandra.dev/find-first-fail' #(s/valid? ::k/create-table %) kasandra.dev/ctables-)
  (s/explain ::k/create-table (first kasandra.dev/ctables-))

  )
