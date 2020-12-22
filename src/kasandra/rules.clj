(ns kasandra.rules
  (:require [clojure.set :as set]
            [clara.rules :as rules]
            [kasandra.schema :as k]
            [kasandra.rules
             [intents :as intents]
             [effects :as fx]
             [relations :as relations]])
  (:import [kasandra.rules.effects
            Effect
            AddTableColumns
            DropTableColumn
            CreateTable
            DropTable
            DropView
            CreateView]
           [kasandra.rules.intents
            AddTableColumnsIntent
            RemoveTableColumnsIntent
            CreateTableIntent
            DropTableIntent
            CreateViewIntent
            DropViewIntent]
           [kasandra.rules.relations
            ;Relation
            TableViewRelation
            UniqueKeyTableRelation
            SecondaryTableRelation
            LookupTableRelation]))

;;; utils

(defn intersect-columns
  [table add-columns]
  (set/intersection (set (k/column-names table))
                    (set (map :name (:columns add-columns)))))

(defn intersect-columns*
  [table remove-cols]
  (set/intersection (set (k/column-names table))
                    (set (:columns remove-cols))))

(defn add-new-columns-to-view
  [view add-table-cols-effect]
  {:pre [(= :kasandra.schema/materialized-view (type view))]}
  (update view :kasandra.schema/view-columns
          #(reduce (fn [cols col] (conj cols col))
                   %
                   (map :name (:columns add-table-cols-effect)))))

(defn remove-columns-from-view
  [view remove-table-cols-intent]
  {:pre [(= :kasandra.schema/materialized-view (type view))]}
  (let [cols-to-drop (set (:columns remove-table-cols-intent))]
    (update view :kasandra.schema/view-columns
            #(vec (filter (fn [col] (not (contains? cols-to-drop col))) %)))))

(defn columns-in-primary-key?
  [view remove-cols-intent]
  (not (empty? (set/intersection (set (k/primary-key-column-names view))
                                 (set (:columns remove-cols-intent))))))

;;; internal

(defrecord ^:private RecreateView [^String view])

;;; rules

(defprotocol Problem)

(defrecord TableAlreadyExists [table-name]
  Problem)

(defrecord ViewAlreadyExists [view-name]
  Problem)

(defrecord ColumnsNotPresent [table-name columns]
  Problem)

(defrecord ColumnExists [table-name clashing-columns]
  Problem)

(defrecord ColumnsInPrimaryKey [view-name columns]
  Problem)

(defrecord ColumnsInDerivedTable [table-name columns]
  Problem)

(def ^:const order-before :kasandra.rules.effects.order/before)

(def ^:const order-after :kasandra.rules.effects.order/after)

(rules/defrule table-view-relation      ; *
  [:kasandra.schema/table [{table-name :kasandra.schema/table :as table}]
   (= ?table-name table-name)]
  [:kasandra.schema/materialized-view [{view-name  :kasandra.schema/view
                                        base-table-name :kasandra.schema/base-table}]
   (= ?view-name view-name)
   (= base-table-name ?table-name)]
  =>
  (rules/insert! (relations/->TableViewRelation ?table-name ?view-name)))

(rules/defrule add-column-to-table      ; *
  [?add-table-cols <- AddTableColumnsIntent (= ?table-name table)]
  [?table-info <- :kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?table table-name)]
  [:test (not (empty? (:columns ?add-table-cols)))]
  [:test (empty? (intersect-columns ?table-info ?add-table-cols))]
  =>
  (rules/insert! (fx/map->AddTableColumns ?add-table-cols)))

(rules/defrule problem-when-adding-column-clashing-name ; *
  [?add-table-cols <- AddTableColumnsIntent (= ?table-name table)]
  [?table-info <- :kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?table table-name)]
  [:test (not (empty? (intersect-columns ?table-info ?add-table-cols)))]
  =>
  (rules/insert! (->ColumnExists ?table-name (intersect-columns ?table-info ?add-table-cols))))

(rules/defrule add-columns-to-secondary-tables ; :after
  [?add-table-cols <- AddTableColumns (= ?table-name table)]
  [SecondaryTableRelation (= ?table-name table) (= ?secondary-table secondary-table)]
  [:kasandra.schema/table [{secondary-table :kasandra.schema/table}] (= secondary-table ?secondary-table)]
  =>
  (rules/insert! (fx/map->AddTableColumns {:table ?secondary-table
                                           :columns (:columns ?add-table-cols)
                                           ::fx/order order-after})))

(rules/defrule remove-columns-from-secondary-tables ; :after
  [?remove-table-col <- DropTableColumn (= ?table-name table)]
  [SecondaryTableRelation (= ?table-name table) (= ?secondary-table secondary-table)]
  [:kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= table-name ?secondary-table)]
  ;;TODO: check if col is in secondary table's primary key OR separate rule for that->Problem?
  =>
  (rules/insert! (fx/map->DropTableColumn {:table ?secondary-table
                                           :column (:column ?remove-table-col)
                                           ::fx/order order-after})))

(rules/defrule create-table             ; *
  [?intent <- CreateTableIntent]
  =>
  (rules/insert! (fx/map->CreateTable ?intent)))

(rules/defrule table-alread-exists      ; *
  [CreateTableIntent (= ?table-name (:kasandra.schema/table definition))]
  [:kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?table-name table-name)]
  =>
  (rules/insert! (->TableAlreadyExists ?table-name)))

(rules/defrule create-view              ; *
  [?intent <- CreateViewIntent]
  =>
  (rules/insert! (fx/map->CreateView ?intent)))

(rules/defrule view-already-exists      ; *
  [CreateViewIntent (= ?view-name (:kasandra.schema/view definition))]
  [:kasandra.schema/materialized-view [{view-name :kasandra.schema/view}]
   (= ?view-name view-name)]
  =>
  (rules/insert! (->ViewAlreadyExists ?view-name)))

(rules/defrule drop-table               ; *
  [DropTableIntent (= ?table-name table)]
  [:kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?table-name table-name)]
  =>
  (rules/insert! (fx/->DropTable ?table-name)))

(rules/defrule cascade-table-drop-to-views ; :after
  [DropTable (= ?table-name table)]
  [?view <- :kasandra.schema/materialized-view [{base-table :kasandra.schema/base-table}]
   (= ?table-name base-table)]
  =>
  (rules/insert! (fx/map->DropView {:view (:kasandra.schema/view ?view)
                                    ::fx/order order-after})))

(rules/defrule cascade-table-drop-to-secondary-tables ; :after
  [DropTable (= ?table-name table)]
  [:or
   [LookupTableRelation (= ?table-name table) (= ?secondary-table lookup-table)]
   [SecondaryTableRelation (= ?table-name table) (= ?secondary-table secondary-table)]
   [UniqueKeyTableRelation (= ?table-name table) (= ?secondary-table other-table)]]
  [?secondary <- :kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?secondary-table table-name)]
  =>
  (rules/insert! (fx/map->DropTable {:table ?secondary-table
                                     ::fx/order order-after})))

(rules/defrule drop-column-from-table   ; *
  [?remove-table-cols <- RemoveTableColumnsIntent (= ?table-name table)]
  [?table-info <- :kasandra.schema/table [{table-name :kasandra.schema/table}]
   (= ?table-name table-name)]
  =>
  (doseq [col (:columns ?remove-table-cols)]
    (rules/insert! (fx/->DropTableColumn ?table-name col))))

(rules/defrule cascade-drop-column-from-table-to-view ; before + :after
  [?remove-table-cols <- RemoveTableColumnsIntent (= ?table-name table)]
  [SecondaryTableRelation (= table ?table-name) (= ?secondary-table secondary-table)]
  [?view-info <- :kasandra.schema/materialized-view [{view-name :kasandra.schema/view
                                                      base-table :kasandra.schema/base-table}]
   (= ?table-name base-table)
   (= ?secondary-table view-name)]
  [:test (not (empty? (intersect-columns* ?view-info ?remove-table-cols)))]
  =>
  (rules/insert! (fx/map->DropView {:view (:kasandra.schema/view ?view-info)
                                    ::fx/order order-before}))
  (rules/insert! (fx/map->CreateView {:definition (remove-columns-from-view ?view-info
                                                                            ?remove-table-cols)
                                      ::fx/order order-after}))
  (rules/insert! (->RecreateView (:kasandra.schema/view ?view-info))))

(rules/defrule cascade-add-column-from-table-to-view ; :before + after
  [?add-table-cols <- AddTableColumnsIntent (= ?table-name table)]
  [SecondaryTableRelation (= table ?table-name) (= ?secondary-table secondary-table)]
  [?view-info <- :kasandra.schema/materialized-view [{view-name :kasandra.schema/view
                                                      base-table :kasandra.schema/base-table}]
   (= ?table-name base-table)
   (= ?secondary-table view-name)]
  [:test (empty? (intersect-columns ?view-info ?add-table-cols))]
  =>
  (rules/insert! (fx/map->DropView {:view (:kasandra.schema/view ?view-info)
                                    ::fx/order order-before}))
  (rules/insert! (fx/map->CreateView {:definition (update ?view-info :kasandra.schema/view-columns into (map :name (:columns ?add-table-cols)))
                                      ::fx/order order-after}))
  (rules/insert! (->RecreateView (:kasandra.schema/view ?view-info))))

(rules/defrule problem-when-dropping-columns-in-view-primary-key ; *
  [?remove-table-cols <- RemoveTableColumnsIntent (= ?table-name table)]
  [RecreateView (= ?view-name view)]
  [?view-info <- :kasandra.schema/materialized-view [{view-name :kasandra.schema/view}]
   (= ?view-name view-name)]
  [:test (columns-in-primary-key? ?view-info ?remove-table-cols)]
  =>
  (rules/insert! (->ColumnsInPrimaryKey ?view-name (:columns ?remove-table-cols))))

(rules/defrule problem-when-dropping-nonexistent-columns ; *
  [RemoveTableColumnsIntent
   (= ?table-name table)
   (= ?columns-to-drop columns)]
  [?table <- :kasandra.schema/table [{table-name :kasandra.schema/table
                                      columns :kasandra.schema/columns}]
   (= ?table-name table-name)
   (= ?columns columns)]
  [:test (not (empty? (set/difference (set ?columns-to-drop)
                                      (set (k/column-names ?table)))))]
  =>
  (rules/insert! (->ColumnsNotPresent ?table-name
                                      (set/difference (set ?columns-to-drop)
                                                      (set (k/column-names ?table))))))

(rules/defrule problem-when-removing-columns-from-lookup+unique-key-tables ; *
  [?remove-cols <- RemoveTableColumnsIntent
   (= ?table-name table)
   (= ?columns-to-drop columns)]
  [:or
   ;; SecondaryTables can remove cols, handled elsewhere
   [LookupTableRelation (= ?table-name table) (= ?secondary-table lookup-table)]
   [UniqueKeyTableRelation (= ?table-name table) (= ?secondary-table other-table)]]
  [?table <- :kasandra.schema/table [{columns :kasandra.schema/columns}]]
  [:test (not (empty? (intersect-columns* ?table ?remove-cols)))]
  =>
  (rules/insert! (->ColumnsInDerivedTable ?secondary-table (:columns ?remove-cols))))

(rules/defquery create-views
  []
  [?create-view <- CreateView])

(rules/defquery drop-views
  []
  [?drop-view <- DropView])

(rules/defquery effects
  []
  [?effect <- Effect])

(rules/defquery problems
  []
  [?problem <- kasandra.rules.Problem])

(rules/defquery relations
  []
  [?relation <- kasandra.rules.relations.Relation])

(rules/defquery intents
  []
  [?intent <- kasandra.rules.intents.Intent])

;;; add rule/heuristic for adding new column to a view, where the view column list == table column list

;; (rules/defrule add-column-to-table
;;   [AddTableColumn (= ?table-name table)]
;;   =>
;;   (rules/insert! ))


;; (rules/defquery table-view-relations
;;   [:?table]
;;   [?relation <- TableViewRelation (= ?table table)])

(rules/defquery views-of-table
  [:?table]
  [?view <- :kasandra.schema/materialized-view [{base-table :kasandra.schema/base-table}]
   (= ?table base-table)])

;;;

(defmulti -session-from
  "Fill session with fact using the supplied object"
  (fn [sess from] (type from)))

(defn make-session
  "Creates a session.

  Supported options:
  - :from - will be used to populate session with facts.

  Implementation to populate session with information from 
  :kasandra.schema/schema-info is provided."
  ([]
   (rules/mk-session 'kasandra.rules))
  ([opts]
   (let [{from :from} opts]
     (cond-> (rules/mk-session 'kasandra.rules)
       (some? from) (-session-from from)))))

(defn insert
  [session & facts]
  (apply rules/insert session facts))

(defn fire-rules
  [session]
  (rules/fire-rules session))

(defn- query-as-set
  [session query k]
  (->> (rules/query session query)
       (map k)
       (set)))

(defn summarize
  "Returns a map of {:effects ... :problems ...} for given session."
  ([session]
   (let [problems (query-as-set session problems :?problem)
         effects (query-as-set session effects :?effect)]
     {:effects effects
      :problems problems})))

(defn summarize-or-throw
  "Returns a map containing :effects. Throws when found any problems."
  [session]
  (let [{effects :effects problems :problems} (summarize session)]
    (when-not (empty? problems)
      (let [intents (query-as-set session intents :?intent)]
        (throw (ex-info "Intent conflicts with defined rules"
                        {:intents intents
                         :problems problems}))))
    {:effects effects}))


(defn run-session-for
  "Runs rules in a new session for given schema-info and intent.
  Returns a summary containing :effects and :problems.

  Provided as a convinience, when you don't need access to the
  underlying session.
  
  See [[summarize]]."
  [schema-info intent]
  (-> (make-session {:from schema-info})
      (insert intent)
      (fire-rules)
      (summarize)))

(defmethod -session-from :kasandra.schema/schema-info [session schema-info]
  (apply insert session (concat (:tables schema-info)
                                (:views schema-info))))

(comment

  "just a query test query; syntax sanity check"
  (rules/defquery views-of-table++
    [:?table]
    [TableViewRelation (= ?table table) (= ?view-name view)]
    [?target-view <- :kasandra.schema/materialized-view [{base-table :kasandra.schema/base-table
                                                          view :kasandra.schema/view}]
     (= ?view-name view)]))

