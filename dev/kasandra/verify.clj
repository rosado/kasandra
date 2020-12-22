(ns kasandra.verify
  (:require [kasandra.schema :as k]
            [clojure.pprint :as pprint]))


(defn parse-cql
  [{file :file}]
  {:pre [(symbol? file)]}
  (let [{:keys [tables views]} (k/schema-info (k/->MatchBasedSchemaInfoProvider)
                                              (slurp (name file)))]
    (pprint/pprint {:tables (count tables)
                    :views (count views)})))
