(ns kasandra.datalog
  (:require [clojure.core.match :as match]
            [datascript.core :as d]
            [kasandra.schema :as k]))


;;; TODO: we need functions that trasform kasandra format table/view definitions
;;; into datoms. Add those datoms to the DB in a transaction and then query

(comment
  (do ;;; TABLES
    (def users-table
      ^{:type ::k/table}
      #::k{:type ::k/table
           :table "users"
           :columns [["id " {:type "uuid"}]
                     ["name" {:type "text"}]
                     ["email" {:type "text"}]]
           :primary-key {::k/partition-key ["id"]}})

    (def orgs-table
      ^{:type ::k/table}
      #::k{:table "orgs"
           :columns [["id" {:type "uuid"}]
                     ["name" {:type "text"}]
                     ["config" {:type "text"}]]
           :primary-key {::k/partition-key ["id"]}})

    (def org-users-table
      ^{:type ::k/table}
      #::k{:table "org_users"
           :columns [["org_id" {:type "uuid"}]
                     ["user_id" {:type "uuid"}]
                     ["name" {:type "text"}]
                     ["org_email" {:type "text"}]]
           :primary-key {::k/partition-key ["org_id" "user_id"]}}))

  )

(defn make-id-provider
  "Returns a fn.
  `ids` should be a map where keys are tuples like
  - [:table \"orgs\"] - for orgs
  - [:column \"orgs\" \"org_id\"] - for columns"
  [ids]
  (let [state (atom {:counter -1 :ids ids})]
    (fn [item]
      (println "current state" @state (get-in (:ids @state) item))
      (if-let [id (get (:ids @state) item)]
        id
        (let [{next-free :counter} @state]
          (swap! state (fn [{:keys [counter ids] :as m}]
                         (-> m
                             (update :counter dec)
                             (update :ids assoc item counter))))
          next-free)))))

(defn- find-pos [k ks]
  (->> ks
       (keep-indexed (fn [index k']
                       (when (= k k')
                         index)))
       first))

(defn- to-col-schema
  [col-key partition-keys clustering-keys id-provider]
  (let [part-key-pos (find-pos col-key partition-keys)
        clust-key-pos (when-not part-key-pos
                        (find-pos col-key clustering-keys))]
    (cond-> {:c*.table.column/name (keyword col-key)}
      (some? part-key-pos) (assoc :c*.table.partition-key/position part-key-pos)
      (some? clust-key-pos) (assoc :c*.table.clustering-key/position clust-key-pos))))

(defn -columns
  [id-provider table]
  (assert (= ::k/table (type table)))
  (let [{::k/keys [primary-key] table-name ::k/table} table
        {::k/keys [partition-key clustering-key]} primary-key]
    (for [column (k/column-names table)]
      (assoc (to-col-schema column partition-key clustering-key id-provider)
             :db/id (id-provider [:column table-name column])))))

(defn -table
  [id-provider table]
  (let [columns (-columns id-provider table)
        {::k/keys [table]} table]
    {:db/id (id-provider [:table (::k/table table)])
     :c*.table/name (keyword table)
     :c*.table/columns (set (map :db/id columns))}))



(comment
  (def ID-PROVIDER (make-id-provider {}))
  (-table ID-PROVIDER org-users-table)
  (-columns ID-PROVIDER org-users-table)
  ({:c*.table.column/name :org_id,
    :c*.table.partition-key/position 0,
    :db/id -2}
   {:c*.table.column/name :user_id, :c*.table.partition-key/position 1, :db/id -3} {:c*.table.column/name :name, :db/id -4} {:c*.table.column/name :org_email, :db/id -5})

  (ID-PROVIDER [:table "orgs"])

  ;; imaginary api ???

  (pk-for app ::org #pk.query [:org-user org-user])
  (pk-for app ::conversation-message #pk.query [:conversation conversation])

  )

 
