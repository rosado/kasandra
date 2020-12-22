(ns kasandra.rules.integration-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as exp]
            [kasandra.schema :as k]
            [kasandra.specs]
            [kasandra.rules :as rules]
            [kasandra.rules
             [intents :as intents]
             [effects :as fx]
             [relations :as relations]]
            [kasandra.rules.effects.application :as application]))


(def users-table
  ^{:type ::k/table}
  #::k{:type ::k/table
       :table "users"
       :columns [["org_id" {:type "uuid"}]
                 ["user_id" {:type "uuid"}]
                 ["name" {:type "text"}]
                 ["role" {:type "text"}]
                 ["updated_at" {:type "timestamp"}]
                 ["username" {:type "text"}]]
       :primary-key {::k/partition-key ["org_id" "user_id"]}
       :keyspace "integration_ks"
       :table-options []})

(assert (s/valid? ::k/create-table users-table))

(def users-full-by-user-id-view
  ^{:type ::k/materialized-view}
  #::k{:type ::k/create-materialized-view
       :view "users_full_by_name"
       :base-table "users"
       :view-columns (k/column-names users-table)
       :primary-key {::k/partition-key ["org_id"]
                     ::k/clustering-key [["user_id" :asc]]}})

(assert (s/valid? ::k/create-materialized-view users-full-by-user-id-view))

(def empty-schema-info
  (let [schema-info ^{:type ::k/schema-info} {:tables [] :views []}]
    (k/add-index schema-info)))

(defn run-session
  ([initial-tables-and-views intent]
   (run-session initial-tables-and-views [] intent))
  ([initial-tables-and-views relations intent]
   (let [session (-> (apply rules/insert (rules/make-session)
                            (concat initial-tables-and-views relations))
                     (rules/insert intent)
                     (rules/fire-rules))]
     (rules/summarize-or-throw session))))


(deftest full-run
  (let [{fx :effects} (run-session [] (intents/->CreateTableIntent users-table))
        schema-info' (application/apply-effects empty-schema-info fx)]
    (is (k/lookup-by-name schema-info' "users"))

    (testing "Add a view. "
      (let [secondary-table-rel (relations/->SecondaryTableRelation (::k/table users-table)
                                                                    (::k/view users-full-by-user-id-view))
            {fx :effects} (run-session (concat (:tables schema-info')
                                               (:views schema-info'))
                                       [secondary-table-rel]
                                       (intents/->CreateViewIntent users-full-by-user-id-view))
            schema-with-view (application/apply-effects schema-info' fx)]
        (is (k/lookup-by-name schema-with-view "users_full_by_name"))

        (testing "Then add a column to table."
          (let [{fx :effects} (run-session (concat (:tables schema-with-view)
                                                   (:views schema-with-view))
                                           [secondary-table-rel]
                                           (intents/->AddTableColumnsIntent (::k/table users-table)
                                                                            [{:name "started_at"
                                                                              :type "timestamp"}]))
                schema-with-view' (application/apply-effects schema-with-view fx)
                table (k/lookup-by-name schema-with-view' (::k/table users-table))
                view (k/lookup-by-name schema-with-view' (::k/view users-full-by-user-id-view))]
            (is (contains? (set (k/column-names table)) "started_at"))
            (is (contains? (set (k/column-names view)) "started_at"))


            (testing "Then drop that column. "
              (let [{fx :effects} (run-session (concat (:tables schema-with-view')
                                                       (:views schema-with-view'))
                                               [secondary-table-rel]
                                               (intents/->RemoveTableColumnsIntent (::k/table users-table)
                                                                                   ["started_at"]))
                    schema-drop-col (application/apply-effects schema-with-view' fx)]
                (is (= schema-with-view schema-drop-col))

                (testing "And then drop the table."
                  (let [{fx :effects} (run-session (concat (:tables schema-drop-col)
                                                           (:views schema-drop-col))
                                                   [secondary-table-rel]
                                                   (intents/->DropTableIntent (::k/table users-table)))
                        empty-schema (application/apply-effects schema-drop-col fx)]
                    (is (= empty-schema-info empty-schema))))))))))))
