(ns kasandra.test-data
  "Test data")

(def table-no-options
  "create table foobar_ks.foobar (
 org_id uuid,
 id uuid,
 name text,
 primary key ((org_id, id)))")

(def table-no-ks
  "create table foobar (
 org_id uuid,
 id uuid,
 name text,
 primary key ((org_id, id)))")

(def table+options-no-clustering-key "create table foobar_ks.foobar (
 org_id uuid,
 id uuid,
 name text,
 primary key ((org_id, id)))
 WITH bloom_filter_fp_chance = 0.1")

(def some-table
  ;; note: this table is not valid (e.g. primary key & clustring order defs)
  ;; but allows us to test most of the code matchers
  (.trim "
CREATE KEYSPACE foobar_ks WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;

create table foobar_ks.foobar (
 org_id uuid,
 id uuid,
 tags set<text>,
 indices map<text, boolean>,
 name text,
 primary key ((org_id, id)))
WITH CLUSTERING ORDER BY (user_id ASC, id ASC)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}"))

(def table+clustering-key
  (.trim "
CREATE KEYSPACE foobar_ks WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}  AND durable_writes = true;

create table foobar_ks.foobar (
 org_id uuid,
 id uuid,
 name text,
 primary key ((org_id, id), name))
WITH CLUSTERING ORDER BY (name ASC)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}"))

(def table-inline-primary-key
  "create table foobar (id int primary key, body text)")

(def table-with-primary-key-1
  "create table accounts (
org_id uuid,
id uuid,
name text,
perms text,
primary key (org_id, id, name))
    with clustering order by (id desc, name asc)
    AND bloom_filter_fp_chance = 0.01
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}")

(def materialized-view-1
  "
CREATE MATERIALIZED VIEW ks_test.activity_by_message  AS
    SELECT org_id, conversation_id, message_id, user_id, notification_timeuuid, details, has_badge, muted, notification_time, read
    FROM ks_test.org_user_activities_by_conversation
    WHERE org_id IS NOT NULL AND conversation_id IS NOT NULL AND message_id IS NOT NULL AND user_id IS NOT NULL AND notification_timeuuid IS NOT NULL
    PRIMARY KEY ((org_id, conversation_id), message_id, user_id, notification_timeuuid)
    WITH CLUSTERING ORDER BY (message_id ASC, user_id ASC, notification_timeuuid ASC)
    AND bloom_filter_fp_chance = 0.1
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}
    AND comment = ''
    AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy', 'tombstone_compaction_interval': '86400'}
    AND compression = {'chunk_length_in_kb': '64', 'class': 'org.apache.cassandra.io.compress.LZ4Compressor'}
    AND crc_check_chance = 1.0
    AND dclocal_read_repair_chance = 0.1
    AND default_time_to_live = 0
    AND gc_grace_seconds = 864000
    AND max_index_interval = 2048
    AND memtable_flush_period_in_ms = 0
    AND min_index_interval = 128
    AND read_repair_chance = 0.0
    AND speculative_retry = '99PERCENTILE';

")
