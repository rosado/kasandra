(ns kasandra.rules.intents)

(defprotocol Intent
  "Marker protocol.")

(defrecord AddTableColumnsIntent [^String table columns]  ;columns: seq of {:name sting :type {:type ...}}
  Intent)

(defrecord RemoveTableColumnsIntent [^String table columns] ;columns: seq string
  Intent)

(defrecord CreateTableIntent [definition]
  Intent)

(defrecord DropTableIntent [^String table])

(defrecord CreateViewIntent [definition])

(defrecord DropViewIntent [^String view])

(comment
  ;; and maybe...
  AddViewColumn ;; drop + create
  DropViewColumn ;; drop + create
  )
