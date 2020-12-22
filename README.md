# kasandra

Utilities for Apache Cassandra schema manipulation for Clojure.

The main ideas are:

1. Read an existing CQL schema (e.g. output of `describe YOUR_KEYSPACE` in cqlsh) to create an in-memory representation that we can easily manipulate in the REPL.
2. Feed that representation of tables and views to the rules engine (as *facts*).
2. add more facts to the rules engine, e.g. an `Intent` to add a column to a table or some of the defined `Relation`s (e.g. to hint that table A' is a lookup table for table A).
4. produce a set of `Effect`s that can be easily applied to the in-memory schema or an actual Cassandra DB.

## Goals

- help remove tedious/erroneous work around migrations
- automate creation of 'down' migrations
- enable exploratory work on DB schema in the REPL

### example task: Add a new Entity, backed by a new table.

To add the entity we first need to create a table in the DB (so, a migration is needed). We haven't defined the entity in our system yet and the rules engine doesn't know about it.

During the our REPL session we want to be able to:
- feed intents to rules engine
- inspect the result of firing the rules
- apply the resulting effects to the in memory represenation of the DB (aka schema-info)
- apply the resulting effects to Cassandra DB
- we also want an easy way to rollback the changes (from both schema-info and Cassandra).

```
let s  : schema-info
let s' : schema-info
s' = apply(s, fx)
```

To automatically create a "down" migration (rollback), we need to always feed `s'` to the rules engine and create intents inverse to the ones we used in "up" migration.

*Note*: an alternative to modifying schema-info structure is to apply fx to the rules engine. I've chosen against this option to minimize interactions with the rules engine itself.

## Status

This is alpha software, an experiment, public API is not final.

Cassandra's User Defined Types are not supported yet.

## Usage

The example below we express the intent of adding a column to a table and apply the resulting effects to `schema-info` (an in-memory representation of a Cassandra schema). 

```clj
(require '[kasandra.schema :as k])
(require '[kasandra.rules :as rules])
(require '[kasandra.rules.intents :as intents])
(require '[kasandra.rules.effects.application :as fx.application])

(let [cql (slurp "path-to-schema.cql")
      schema-info (k/schema-info (k/->MatchBasedSchemaInfoProvider) cql)
      intent (intents/->AddTableColumnsIntent "users" [{:name "nick"
                                                        :type {:type "text"}}])
      summary (rules/run-session-for schema-info intent)
      _ (assert (empty? (:problems summary)))]
  (fx.application/apply-effects schema-info (:effects summary)))
```

The effects resulting from `run-session-for` are just Clojure records. We can easily transform them into something that your migration tool can use (in the simplest case, CQL strings).

Note: The last part is not a part of the library at the moment. At the moment I have code that works with codebase I use at work, but is too specific to share it here.

## What's out of scope?

The intention is for this library to not rely on any particular Cassandra driver. That belongs in separate library (maybe with this library as a dependency).

## TODO

- more work on `kasandra.rules.effects.application` - refine the api
- creating 'inverse' intents for 'down' migrations
- utility fns for pulling relations into the Clara session

## License

Copyright © 2020 Roland Sadowski

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
