(ns kasandra.rules.effects
  "Effects is the result of intents.

  # Ordering

  The collection of effects can potentially be grouped into groups determined
  by the value under :effects/order.

  The possible values are
  - `:kasandra.rules.effects.order/before` - should be applied before others
  - (no entry) - should be applied between others
  - `:kasandra.rules.effects.order/after` - should be applied before others. ")

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

