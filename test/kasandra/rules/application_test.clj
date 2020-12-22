(ns kasandra.rules.application-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as exp]
            [kasandra.schema :as k]
            [kasandra.specs]
            [kasandra.rules :as rules]
            [kasandra.rules [effects :as effects]]
            [kasandra.rules.effects.application :as application]))


(def table-a1
  ^{:type ::k/table}
  #::k{:type ::k/table
       :table "a1"
       :columns [["id "{:type "uuid"}]
                 ["label"{:type "text"}]
                 ["ts" {:type "timestamp"}]]
       :primary-key {::k/partition-key ["id"]}})

(assert (s/valid? ::k/create-table table-a1))

(def view-for-table-a1
  ^{:type ::k/materialized-view}
  #::k{:type ::k/create-materialized-view
       :view "a1_view"
       :base-table "a1"
       :view-columns ["id" "label" "ts"]
       :primary-key {::k/partition-key ["label"]
                     ::k/clustering-key [["ts" :asc]]}})

(assert (s/valid? ::k/create-materialized-view view-for-table-a1))

(deftest create-and-drop-table-test
  (let [schema-info ^{:type ::k/schema-info } {:tables [table-a1] :views []}
        schema-info (k/add-index schema-info)]
    (testing "adding a table"
      (let [result (application/DEBUG-apply-effect schema-info
                                                   (effects/->CreateTable
                                                    (assoc table-a1 ::k/table "b1")))]
        (is (= #{"a1" "b1"}
               (set (keys (:index result)))))
        
        (testing "and then dropping it"
          (let [result (application/DEBUG-apply-effect result
                                                       (effects/->DropTable "b1"))]
            (is (= #{"a1"} (set (keys (:index result)))))))))))

(deftest column-add-and-drop
  (let [schema-info ^{:type ::k/schema-info } {:tables [table-a1] :views []}
        schema-info (k/add-index schema-info)]
    (testing "adding a column"
      (let [result (application/apply-effects schema-info
                                              [(effects/->AddTableColumns "a1"
                                                                          [{:name "new_col"
                                                                            :type "int"}])])]
        (is (= (conj (set (k/column-names (k/lookup-by-name schema-info "a1"))) "new_col")
               (set (k/column-names (k/lookup-by-name result "a1")))))
        (testing "and dropping it"
          (let [result (application/apply-effects result [(effects/->DropTableColumn "a1" "new_col")])]
            (is (= (set (k/column-names (k/lookup-by-name schema-info "a1")))
                   (set (k/column-names (k/lookup-by-name result "a1")))))))))))

;; (deftest fx-ordering
;;   (let [schema-info ^{:type ::k/schema-info} {:tables [table-a1] :views [view-for-table-a1]}]
;;     ))
