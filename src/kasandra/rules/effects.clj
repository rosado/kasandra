(ns kasandra.rules.effects)

;;; TODO: docs about kasandra.rules.effects/order

(defprotocol Effect
  "Marker protocol.")

(defrecord AddTableColumns [table columns]  ;columns: seq of {:name string :type ???}
  Effect)

(defrecord DropTableColumn [table column]
  Effect)

(defrecord CreateTable [definition]
  Effect) ;definition: conforms to :kasandra.schema/create-table

(defrecord DropTable [^String table]
  Effect)

(defrecord CreateView [definition]      ;definition: conforms to :kasandra.schema/create-materialized-view
  Effect)

(defrecord DropView [^String view]
  Effect)

