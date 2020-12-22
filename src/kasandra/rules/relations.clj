(ns kasandra.rules.relations)

(defprotocol Relation)

(defrecord TableViewRelation [^String table ^String view]
  Relation)

(defrecord UniqueKeyTableRelation [^String table ^String other-table]
  Relation)

(defrecord SecondaryTableRelation [^String table ^String secondary-table]
  Relation)

(defrecord LookupTableRelation [^String table ^String lookup-table]
  Relation)
