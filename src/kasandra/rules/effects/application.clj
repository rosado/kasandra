(ns kasandra.rules.effects.application
  (:require [kasandra.rules.effects :as effects]
            [kasandra.schema :as k])
  (:import [kasandra.rules.effects
            CreateTable
            DropTable
            CreateView
            DropView
            AddTableColumns
            DropTableColumn]))

(defprotocol ^:private Apply
  (-apply [fx schema-info]))

(extend-protocol Apply
  CreateTable
  (-apply [fx schema-info]
    (update schema-info :tables conj (:definition fx)))

  DropTable
  (-apply [fx schema-info]
    (let [{table-name :table} fx]
      (update schema-info
              :tables
              (fn [tables]
                (filterv #(not= (::k/table %) table-name)
                         tables)))))

  AddTableColumns
  (-apply [fx schema-info]
    (let [{table-name :table columns :columns} fx
          columns (map #(vector (:name %) (dissoc % :name)) columns)
          location (get (:index schema-info) table-name)]
      (update-in schema-info location #(update % ::k/columns into columns))))

  DropTableColumn
  (-apply [fx schema-info]
    (let [{table-name :table column :column} fx
          loc (get (:index schema-info) table-name)]
      (update-in schema-info (concat loc [::k/columns])
                 (fn [cols] (filter #(not= (first %) column) cols)))))

  CreateView
  (-apply [fx schema-info]
    (update schema-info :views conj (:definition fx)))

  DropView
  (-apply [fx schema-info]
    (let [{view-name :view} fx]
      (update schema-info
              :views
              (fn [views]
                (filterv #(not= (::k/view %) view-name)
                         views))))))

(defmulti -apply-effect
  "Apply effect to a schema-info object. Returns the updated schema-info."
  (fn [schema-info effect] (type schema-info)))

(defn DEBUG-apply-effect
  "Returns updates schema-info"
  [schema-info effect]
  {:pre [(= (type schema-info) :kasandra.schema/schema-info)
         (some? (:index schema-info))]}
  (let [updated-index (-apply effect schema-info)]
    (assoc updated-index :index (k/schema-index updated-index))))

(defmethod -apply-effect ::k/schema-info [schema-info effect]
  (DEBUG-apply-effect schema-info effect))

(defn apply-effects
  [schema-info effects]
  {:pre [(= :kasandra.schema/schema-info (type schema-info))]
   :post [(= :kasandra.schema/schema-info (type %))]}
  (let [{fx-before :kasandra.rules.effects.order/before
         fx-after :kasandra.rules.effects.order/after
         :as grouped} (group-by :effects/order effects)
        main-fx (get grouped nil)]
    (reduce DEBUG-apply-effect
            schema-info
            (concat fx-before main-fx fx-after))))

;; (defn diff-schema
;;   [left right]
;;   (let [left (dissoc left :index)
;;         right (dissoc right :index)
;;         ]))
