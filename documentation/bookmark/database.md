# Multiversion Concurrency Control

1. [Multiversion Concurrency Control: Theory and Algorithms](http://sungsoo.github.io/papers/bernstein-1983.pdf)
1. []()

# Pagination

1. [Pagination with Relative Cursors](https://shopify.engineering/pagination-relative-cursors)

# Reference

1. ["Temporal Databases for Streaming Architectures" by Jeremy Taylor and Jon Pither](https://www.youtube.com/watch?v=ykbYNBE-V3k)
1. ["Makings of a Modern ACID Compliant Distributed Database" by Attila Szegedi](https://www.youtube.com/watch?v=pn8mCeX3LDE)
1. https://edgedb.com/blog/a-path-to-a-10x-database/
1. https://cstack.github.io/db_tutorial/
1. http://www.sql-workbench.eu/dbms_comparison.html
1. http://www.interdb.jp/pg/index.html
1. [Foundations of Databases](http://webdam.inria.fr/Alice/)
1. https://medium.com/textileio/building-the-firebase-for-crdts-7dd8dea8953a

# Data structure

1. [The Concurrent 2-Trie](https://medium.com/@chrisvest/the-concurrent-2-trie-67deb2b57ba1)

# Query

1. [Reasons why SELECT * is bad for SQL performance](https://tanelpoder.com/posts/reasons-why-select-star-is-bad-for-sql-performance/)
1. [A Short Story About SQL’s Biggest Rival](https://www.holistics.io/blog/quel-vs-sql/)
1. https://calcite.apache.org/
1. https://juxt.pro/blog/crux-sql
1. https://www.influxdata.com/blog/why-were-building-flux-a-new-data-scripting-and-query-language/
1. https://crate.io/a/lab-notes-how-we-made-joins-23-thousand-times-faster-part-two/
1. GC: A Graph Caching System for Subgraph/Supergraph Queries

	http://www.vldb.org/pvldb/vol11/p2022-wang.pdf
1. How to Architect a Query Compiler, Revisited

	https://www.cs.purdue.edu/homes/rompf/papers/tahboub-sigmod18.pdf
1. https://gql.today/
1. Everything You Always Wanted to Know About Compiled and Vectorized Queries But Were Afraid to Ask

	http://www.vldb.org/pvldb/vol11/p2209-kersten.pdf
1. https://github.com/efficient/SuRF
1. http://www.try-alf.org/blog/2013-10-21-relations-as-first-class-citizen
1. https://icfp18.sigplan.org/event/hope-2018-papers-finding-fixed-points-faster
1. Froid: Optimization of Imperative Programs in a Relational Database

	http://www.vldb.org/pvldb/vol11/p432-ramachandra.pdf
1. Query Combinators

	https://arxiv.org/abs/1702.08409
1. https://github.com/medmain/deepr
1. https://edgedb.com/blog/we-can-do-better-than-sql/
1. https://github.com/ollef/sixten/blob/master/docs/QueryCompilerDriver.md

# Optimization

1. [Hera - High Efficiency Reliable Access to data stores](https://github.com/paypal/hera)
1. [Relaxed Operator Fusion for In-Memory Databases: Making Compilation, Vectorization, and Prefetching Work Together At Last](https://db.cs.cmu.edu/papers/2017/p1-menon.pdf)
1. https://medium.com/connect-the-dots/optimizing-queries-in-rethinkdb-584d7f660cb
1. https://blog.acolyer.org/2019/01/18/towards-a-hands-free-query-optimizer-through-deep-learning/
1. http://blog.felipe.rs/2019/01/29/demystifying-join-algorithms/
1. https://corecursive.com/030-rethinking-databases-with-jon-gjengset/

# Index

1. https://www.pilosa.com/
1. https://en.wikipedia.org/wiki/Fractal_tree_index
1. [Beating hash tables with trees? The ART-ful radix trie](https://www.the-paper-trail.org/post/art-paper-notes/)
1. https://www.ristret.com/s/gnd4yr/brief_history_log_structured_merge_trees
1. [Getting The Most Out Of Your PostgreSQL Indexes](https://pgdash.io/blog/postgres-indexes.html?p)
1. http://databasearchitects.blogspot.com/2019/05/why-use-learning-when-you-can-fit.html
1. https://priyankvex.wordpress.com/2019/05/19/a-tale-on-concatenated-indexes-master-roshi-and-gokus-fireside-chat/

# Relational Algebra

1. https://slamdata.com/what-is-multidimensional-relational-algebra/
1. https://github.com/slamdata/purescript-mra

# Time traveling

1. https://www.arangodb.com/2018/07/time-traveling-with-graph-databases/

# Data processing
1. https://medium.com/@gauthierleonard/apache-beam-a-unified-programming-model-for-data-processing-pipelines-4de9fb6ede6d

# Philosophy
1. http://dhickey.ie/2016/01/03/commercial-suicide-integration-at-the-database-level/

# Storage

1. [Understanding LSM Trees: What Powers Write-Heavy Databases](https://yetanotherdevblog.com/lsm/)
1. http://www.benstopford.com/2015/02/14/log-structured-merge-trees/
1. A Comparison of Adaptive Radix Trees and Hash Tables

	https://bigdata.uni-saarland.de/publications/ARCD15.pdf
1. HashKV: Enabling Efficient Updates in KV Storage via Hashing

	https://www.usenix.org/system/files/conference/atc18/atc18-chan.pdf
1. https://akumuli.org/akumuli/2018/08/03/high-cardinality-support/
1. Ideal Hash Trees

	https://github.com/papers-we-love/papers-we-love/blob/master/data_structures/ideal-hash-trees.pdf
1. https://clemenswinter.com/2018/08/13/how-read-100s-of-millions-of-records-per-second-from-a-single-disk/
1. https://github.com/vmxdev/tkvdb
1. https://blog.acolyer.org/2018/09/28/columnstore-and-b-tree-are-hybrid-physical-designs-important/
1. http://sergeiturukin.com/2017/06/07/storage-engine-introduction.html
1. https://accumulo.apache.org/
1. http://www.mit.edu/~kepner/D4M/
1. Achieving 100,000,000 database inserts per second using Accumulo and D4M

	http://www.ieee-hpec.org/2014/CD/index_htm_files/FinalPapers/31.pdf
1. Lambda World 2018 - The Radix Trees How IntMap Works - Tikhon Jelvis

	https://www.youtube.com/watch?v=0udjkEiCjog
1. FineLine: Log-structured Transactional Storage and Recovery

	http://www.vldb.org/pvldb/vol11/p2249-sauer.pdf
1. Storm: a fast transactional dataplane forremote data structures

	http://www.cs.technion.ac.il/~dan/papers/storm-systor-2019.pdf

# Exemplar

1. [Irmin](https://irmin.org/)
1. [Database of Databases](https://dbdb.io/)

## General

1. https://github.com/Workiva/eva/
1. https://learndb.net/
1. http://orientdb.com/
1. https://ignite.apache.org/
1. http://www.datomic.com/
1. https://eventsourcing.com/
1. https://geteventstore.com/
1. http://cassandra.apache.org/
1. https://kafka.apache.org/
1. http://basho.com/products/riak-kv/
1. https://www.influxdata.com/time-series-platform/influxdb/
1. https://prestodb.io/
1. https://github.com/mozilla/datomish
1. http://sqream.com/
1. http://pelotondb.io/
1. https://github.com/tidwall/summitdb
1. https://github.com/facebookincubator/beringei

	https://code.facebook.com/posts/952820474848503/beringei-a-high-performance-time-series-storage-engine
1. https://github.com/tonsky/datascript
1. https://github.com/biokoda/actordb
1. https://www.slashdb.com/
1. https://realm.io/
1. https://www.bigchaindb.com/
1. http://samza.apache.org/
1. https://www.umiacs.umd.edu/~zhangyp/papers/IntegriDB.pdf
1. https://github.com/PumpkinDB/PumpkinDB
1. https://symas.com/offerings/lightning-memory-mapped-database/
1. http://gun.js.org/
1. https://open.dgraph.io/post/benchmark-neo4j/

	1. https://github.com/dgraph-io/dgraph
	1. https://dgraph.io/
1. https://www.spreadsheetdb.io/
1. http://www.openio.io/
1. https://github.com/pingcap/tidb
1. http://druid.io/
1. http://hsqldb.org/
1. http://siridb.net/blog/time-series-with-siridb/
1. https://github.com/jankotek/mapdb
1. https://github.com/NationalSecurityAgency/lemongraph
1. https://twobithistory.org/2017/10/07/the-most-important-database.html
1. https://github.com/couchbase/forestdb
1. https://www.nuodb.com/
1. https://edgedb.com/
1. https://www.harperdb.io/
1. https://www.antidotedb.eu/
1. https://tech.marksblogg.com/minimalist-guide-tutorial-foundationdb.html
1. https://www.foundationdb.org/blog/announcing-document-layer/
1. https://www.foundationdb.org/

	1. https://en.wikipedia.org/wiki/FoundationDB
1. https://github.com/orbitdb/orbit-db
1. https://www.aerospike.com/
1. https://sirix.io/

## Data-flow

1. [Noria: data-flow for high-performance web applications](https://github.com/mit-pdos/noria)

## Graph

1. https://github.com/hugegraph/hugegraph
1. https://github.com/Microsoft/GraphEngine
1. https://redislabs.com/blog/release-redisgraph-v1-0-preview/
1. https://github.com/JanusGraph/janusgraph
1. https://www.slideshare.net/RoiLipman/graph-algebra
1. https://juxt.pro/crux/docs/index.html
1. https://github.com/juxt/crux
1. [The Crux of Bitemporality - Jon Pither](https://www.youtube.com/watch?v=3Stja6YUB94)
1. [Bitemporality: More Than a Design Pattern](https://juxt.pro/blog/bitemporality-more-than-a-design-pattern)

## Column

1. https://blog.acolyer.org/2018/09/26/the-design-and-implementation-of-modern-column-oriented-database-systems/

## Log

1. https://code.fb.com/core-data/logdevice-a-distributed-data-store-for-logs/

## Key-Value

1. https://github.com/yahoo/HaloDB
1. https://chronicle.software/products/map/

## Array
1. [Array Databases: Concepts, Standards, Implementations](https://rd-alliance.org/system/files/Array-Databases_final-report.pdf)

## Blockchain

1. https://flur.ee/

## Time-series

1. https://github.com/alpacahq/marketstore
1. https://www.outlyer.com/blog/why-not-to-build-a-time-series-database/

## GPU

1. https://blazingdb.com/#/

## Tree

1. A Novel Method for Representing Hierarchies in a Relational Database Using Bignums and SQLite

	https://s3.amazonaws.com/pub.xhuntley.net/Huntley_Tcl2011.pdf

# CQRS | Event-Sourcing

1. [Introducing Derivative Event Sourcing](https://www.confluent.io/blog/event-sourcing-vs-derivative-event-sourcing-explained)
1. https://blog.wallaroolabs.com/2018/06/implementing-time-windowing-in-an-evented-streaming-system/
1. https://data-artisans.com/blog/a-practical-guide-to-broadcast-state-in-apache-flink
1. https://medium.com/@hugo.oliveira.rocha/what-they-dont-tell-you-about-event-sourcing-6afc23c69e9a
1. https://vvvvalvalval.github.io/posts/2018-11-12-datomic-event-sourcing-without-the-hassle.html
1. https://chriskiehl.com/article/event-sourcing-is-hard
1. https://medium.com/serialized-io/apache-kafka-is-not-for-event-sourcing-81735c3cf5c

# Architecture

1. https://www.sqlite.org/arch.html

# Criticism

1. https://codeburst.io/doing-without-databases-in-the-21st-century-6e25cf495373

# Geo-spatial

1. H3: Uber’s Hexagonal Hierarchical Spatial Index

	https://eng.uber.com/h3/
1. http://s2geometry.io/
1. https://github.com/mourner/flatbush

# Transaction
1. MixT: An embedded, domain-specific language for Mixed-Consistency Transactions

	https://mpmilano.github.io/MixT/
1. The Case for Shared Nothing

	http://db.cs.berkeley.edu/papers/hpts85-nothing.pdf

# Consistency

1. https://jepsen.io/consistency
1. http://muratbuffalo.blogspot.com/2018/08/the-many-faces-of-consistency.html
1. https://fauna.com/blog/consistency-without-clocks-faunadb-transaction-protocol

# P2P Networking

1. https://medium.com/perlin-network/noise-an-opinionated-p2p-networking-stack-for-decentralized-protocols-in-go-bfc6fecf157d
1. https://xorro-p2p.github.io/

# Data Visualization

1. https://www.tadviewer.com/

# Privacy

1. https://blog.acolyer.org/2019/06/17/towards-multiverse-databases/
1. [Towards Multiverse Databases](https://people.csail.mit.edu/malte/pub/papers/2019-hotos-multiversedb.pdf)

# Security

1. [The ZombieLoad Pragmatist: Tips for Surviving in a Post-Meltdown World](https://www.scylladb.com/2019/05/17/the-zombieload-pragmatist/)

# _Temporary cache_

1. https://github.com/pubkey/rxdb
1. https://hackernoon.com/execute-millions-of-sql-statements-in-milliseconds-in-the-browser-with-webassembly-and-web-workers-3e0b25c3f1a6#.w8w8qi8b3
1. https://github.com/kripken/sql.js
1. http://zmtp.org/
1. https://github.com/agentm/project-m36
1. http://graphql.org/
1. https://docs.microsoft.com/en-us/azure/documentdb/documentdb-introduction
1. http://storm.apache.org/
1. http://www.intersystems.com/our-products/cache/cache-overview/
1. https://prometheus.io/
1. http://www.ragic.com/
1. https://github.com/bittorrent/sqltorrent
1. https://github.com/bitnine-oss/agensgraph
1. [Comdb2: Bloomberg’s Highly Available Relational Database System](http://www.vldb.org/pvldb/vol9/p1377-scotti.pdf)
1. https://www.pipelinedb.com/
1. https://fauna.com/home
1. https://fauna.com/blog/a-database-built-like-an-operating-system
1. http://www.timescaledb.com/
1. https://www.dataengineeringpodcast.com/episode-4-scylladb-with-eyal-gutkind/
1. http://quickstep.incubator.apache.org/
1. https://barrel-db.org/
1. https://misfra.me/2016/04/09/tsdb-list/
1. https://github.com/cayleygraph/cayley
1. https://deepstreamhub.com/
1. http://www.spdk.io/
1. https://github.com/pilosa/pilosa
1. http://sophia.systems/
1. https://humio.com/
1. https://medium.com/@uwdb/introducing-cosette-527898504bd6
1. https://mixmax.com/blog/bee-queue-v1-node-redis-queue
1. http://btrdb.io/
1. https://clickhouse.yandex/
1. https://github.com/rqlite/rqlite
1. https://www.confluent.io/blog/ksql-open-source-streaming-sql-for-apache-kafka/
1. https://github.com/Tencent/paxosstore
1. https://www.ibm.com/us-en/marketplace/project-eventstore
1. [LogDevice: a distributed data store for logs](https://code.facebook.com/posts/357056558062811)
1. https://crate.io/
1. http://traildb.io/
1. http://profanedb.gitlab.io/
1. https://github.com/PyroclastIO/metamorphic
1. https://franchise.cloud/
1. https://github.com/ssbc/secure-scuttlebutt
1. ["Datafun: a functional query language" by Michael Arntzenius](https://www.youtube.com/watch?v=gC295d3V9gE)
1. https://github.com/neo4j-contrib/spatial
1. https://github.com/agershun/alasql
1. http://www.tiledb.io/
1. https://databricks.com/product/databricks-delta
1. http://www.geomesa.org/
1. https://bookkeeper.apache.org/
1. https://blog.outlyer.com/top10-open-source-time-series-databases
1. https://github.com/qubole/rubix
1. https://github.com/confluentinc/ksql
1. https://www.datanami.com/2018/06/15/streaming-sql-for-real-time-analytics/
1. https://blog.fauna.com/fauna-topology-operations
1. https://fabianlindfors.se/blog/building-an-object-store-with-foundation-db/
1. https://use-the-index-luke.com/no-offset
1. [GOTO 2016 • Conflict Resolution for Eventual Consistency • Martin Kleppmann](https://www.youtube.com/watch?v=yCcWpzY8dIA)
1. https://www.thoughts-on-java.org/5-common-hibernate-mistakes-that-cause-dozens-of-unexpected-queries/
1. https://www.nicolaferraro.me/2018/04/25/saga-pattern-in-apache-camel/
1. http://augustl.com/blog/2018/datomic_look_at_all_the_things_i_am_not_doing/
1. https://blog.2ndquadrant.com/application-users-vs-row-level-security/
1. https://medium.com/databasss/on-ways-to-agree-part-1-links-and-flp-impossibility-f6bd8a6a0980
1. https://medium.com/@robot_dreams/disk-can-be-faster-than-memory-37c57e7ebca7
1. http://www.bailis.org/blog/when-is-acid-acid-rarely/
1. https://hackernoon.com/five-data-models-for-sharding-and-which-is-right-f5c3fa480b00
1. http://www.aosabook.org/en/500L/an-archaeology-inspired-database.html
1. https://www.clever-cloud.com/blog/engineering/2015/05/20/why-auto-increment-is-a-terrible-idea/
1. https://samrat.me/posts/2017-11-04-kvstore-linear-hashing/
1. https://detect.io/news/2017/10/16/three-time-series-that-defeat-typical-anomaly-detection
1. http://blog.memsql.com/scaling-distributed-joins/
1. http://akumuli.org/akumuli/2017/11/17/indexing/
1. https://10clouds.com/blog/postgresql-10/
1. https://cstack.github.io/db_tutorial/
1. https://msdn.microsoft.com/en-us/library/jj591569.aspx
1. https://chatwithengineers.com/2016/08/29/a-survey-of-query-execution-engines-from-volcano-to-vectorized-processing/
1. https://martin.kleppmann.com/2015/05/11/please-stop-calling-databases-cp-or-ap.html
1. https://medium.com/the-hoard/building-a-kafka-that-doesnt-depend-on-zookeeper-2c4701b6e961#.paa4pjdtc
1. https://blog.acolyer.org/2017/01/23/ground-a-data-context-service/
1. https://engineering.instagram.com/sharding-ids-at-instagram-1cf5a71e5a5c#.omhi4kyaj
1. http://rystsov.info/2017/02/15/simple-consensus.html
1. https://blog.jooq.org/2016/10/05/why-you-should-design-your-database-to-optimise-for-statistics/
1. https://thesquareplanet.com/blog/students-guide-to-raft/
1. https://rjuju.github.io/postgresql/2015/07/02/how-about-hypothetical-indexes.html
1. http://amitkapila16.blogspot.com/2017/03/hash-indexes-are-faster-than-btree.html
1. http://rhaas.blogspot.com/2017/04/new-features-coming-in-postgresql-10.html
1. https://fabxc.org/blog/2017-04-10-writing-a-tsdb/#
1. https://github.com/confluentinc/bottledwater-pg
1. http://www.postgresonline.com/journal/archives/173-Using-LTree-to-Represent-and-Query-Hierarchy-and-Tree-Structures.html
1. https://juxt.pro/blog/posts/datascript-dom.html
1. https://github.com/agentm/project-m36
1. https://fortytw2.com/relational-dbs-on-kv-store-pt-1
1. https://blog.slicingdice.com/want-to-build-a-new-time-series-database-start-here-d4c3fd1517b1
1. http://akumuli.org/akumuli/2017/04/29/nbplustree/
1. https://www.hugopicado.com/2017/05/06/what-event-sourcing-is-not.html
1. https://news.realm.io/news/conflict-resolution-for-eventual-consistency-goto/
1. https://www.citusdata.com/blog/2017/05/11/dynamo-citus-and-tradeoffs-in-distributed-databases/
1. http://www.cakesolutions.net/teamblogs/how-to-build-a-distributed-counter
1. https://netsil.com/blog/comparison-of-time-series-databases-netsils-use-of-druid/
1. https://www.infoq.com/interviews/meijer-big-data
1. https://www.cs.cmu.edu/~jarulraj/pages/sigmod_2017_tutorial.html
1. https://speakerdeck.com/caitiem20/distributed-sagas-a-protocol-for-coordinating-microservices
1. https://blog.timescale.com/what-the-heck-is-time-series-data-and-why-do-i-need-a-time-series-database-dcf3b1b18563
1. https://medium.com/hyrise/the-brain-of-every-database-c622aaba7d75
1. http://codecapsule.com/2012/11/07/ikvs-implementing-a-key-value-store-table-of-contents/
1. https://www.cockroachlabs.com/blog/sql-in-cockroachdb-mapping-table-data-to-key-value-storage/
1. https://blog.acolyer.org/2017/07/07/do-we-need-specialized-graph-databases-benchmarking-real-time-social-networking-applications/
1. http://vvvvalvalval.github.io/posts/2017-07-08-Datomic-this-is-not-the-history-youre-looking-for.html?t=1&cn=ZmxleGlibGVfcmVjc18y&iid=1e725951a13247d8bdc6fe8c113647d5&uid=2418042062&nid=244+293670920
1. https://maxdemarzi.com/2017/07/13/using-a-cuckoo-filter-for-unique-relationships/
1. https://medium.baqend.com/real-time-databases-explained-why-meteor-rethinkdb-parse-and-firebase-dont-scale-822ff87d2f87
1. http://dawn.cs.stanford.edu/2017/08/07/asap/
1. [TimescaleDB vs. Postgres for time-series: 20x higher inserts, 2000x faster deletes, 1.2x-14,000x faster queries](https://blog.timescale.com/timescaledb-vs-6a696248104e)
1. https://www.elastic.co/blog/elasticsearch-sequence-ids-6-0
1. https://neo4j.com/blog/efficient-graph-algorithms-neo4j/
1. https://blog.jooq.org/2017/05/30/when-to-use-bind-values-and-when-to-use-inline-values-in-sql/
1. https://fylux.github.io/2017/08/09/VAR_Model_Time_Series/
1. https://brandur.org/postgres-atomicity
1. [Generative Programming and Verification - Nada Amin](https://www.youtube.com/watch?v=QuJ-cEvH_oI)
1. http://ithare.com/oltp-db-optimizations-101-thinking-in-terms-of-execution-plans/
1. https://pingcap.github.io/blog/2017/08/15/multi-raft/
1. https://medium.com/@ajstorm/the-lambda-architecture-simplified-a28e436fa55e
1. http://akumuli.org/akumuli/2016/12/30/compression_part1/
1. https://en.wikipedia.org/wiki/Codd%27s_12_rules
1. https://blog.jooq.org/2017/09/28/10-cool-sql-optimisations-that-do-not-depend-on-the-cost-model/
1. https://medium.com/@ifesdjeen/on-disk-storage-part-4-b-trees-30791060741
1. https://www.cockroachlabs.com/blog/automated-rebalance-and-repair/
1. http://danielwhittaker.me/2017/10/09/handle-set-based-consistency-validation-cqrs/
1. http://db.cs.cmu.edu/seminar2017/
1. https://blog.timescale.com/time-series-data-postgresql-10-vs-timescaledb-816ee808bac5
1. [Is Datomic strictly better than Facebook's graph datastore?](http://www.dustingetz.com/ezpjb2RlLWRhdGFiYXNlICJzdGFydGVyLWJsb2ctc3JjMiIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU1NTkgI1VSSSAiZGF0b21pYzpmcmVlOi8vZGF0b21pYzo0MzM0L3N0YXJ0ZXItYmxvZy1zcmMyIl0sIDpxdWVyeS1wYXJhbXMgezplbnRpdHkgI0RiSWRbMTc1OTIxODYwNDYxNDQgI1VSSSAiZGF0b21pYzpmcmVlOi8vZGF0b21pYzo0MzM0L2R1c3RpbmdldHouY29tIl19fQ,,)
1. https://www.citusdata.com/blog/2017/10/17/tour-of-postgres-index-types/
1. https://blog.grakn.ai/modelling-data-with-hypergraphs-edff1e12edf0
1. http://www.scylladb.com/2017/10/05/io-access-methods-scylla/
1. http://code.hootsuite.com/row-stores-vs-column-stores-why-not-use-both/
1. http://matthewrocklin.com/blog/work/2017/10/16/streaming-dataframes-1
1. https://brightinventions.pl/blog/database-timeouts/
1. https://brandur.org/redis-streams
1. https://www.fittedcloud.com/blog/machine-learning-driven-optimization-outperforms-auto-scaling-dynamodb-capacity-management/
1. https://parquet.apache.org/
1. https://arrow.apache.org/
1. https://www.youtube.com/watch?v=6lCVKMQR8Dw
1. https://blog.acolyer.org/2015/10/15/ironfleet-proving-practical-distributed-systems-correc/
1. https://underscore.io/blog/posts/2017/03/07/refined-data-config-database.html
1. https://www.quickdatabasediagrams.com/
1. [The Case for Learned Index Structures](https://arxiv.org/abs/1712.01208)
1. https://arxiv.org/abs/0806.3115
1. https://arxiv.org/abs/1604.03607
1. https://arxiv.org/abs/1702.07588
1. [HoTTSQL: Proving Query Rewrites with Univalent SQL Semantics](https://arxiv.org/abs/1607.04822)
1. http://www.vldb.org/pvldb/vol9/p1707-pirk.pdf
1. [Continuous Analytics Over Discontinuous Streams](https://people.eecs.berkeley.edu/~franklin/Papers/sigmod10krishnamurthy.pdf)
1. http://www.vldb.org/
1. https://blog.acolyer.org/2017/01/30/how-good-are-query-optimizers-really/
1. http://gun.js.org/distributed/matters.html
1. http://blog.datomic.com/2017/01/the-ten-rules-of-schema-growth.html
1. https://blogs.technet.microsoft.com/dataplatforminsider/2017/01/05/how-six-lines-of-code-sql-server-can-bring-deep-learning-to-any-app/
1. https://muratbuffalo.blogspot.com/2014/07/hybrid-logical-clocks.html
1. https://yokota.blog/2017/02/17/dont-settle-for-eventual-consistency/
1. [AN INTRODUCTION TO GRAPH COMPRESSION TECHNIQUES FOR IN-MEMORY GRAPH COMPUTATION](https://www.cs.umd.edu/sites/default/files/scholarly_papers/Chavan.pdf)
1. [Relational Lattices: From Databases to Universal Algebra](http://www.dcs.bbk.ac.uk/~szabolcs/rellat-jlamp-second-submission-2.pdf)
1. https://bitworking.org/news/2017/03/prometheus
1. [Partioned Elias-Fano indexes](http://hpc.isti.cnr.it/hpcworkshop2014/PartitionedEliasFanoIndexes.pdf)
1. http://h2co3.org/blog/index.php/2017/04/10/join-considered-harmful/
1. https://blog.jooq.org/2016/07/05/say-no-to-venn-diagrams-when-explaining-joins/
1. [A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing](http://www.vldb.org/pvldb/vol9/p96-richter.pdf)
1. https://research.googleblog.com/2017/04/consistent-hashing-with-bounded-loads.html
1. [The Adaptive Radix Tree: ARTful Indexing for Main-Memory Databases](https://pdfs.semanticscholar.org/6abf/5107efc723c655956f027b4a67565b048799.pdf)
1. [Adapting Tree Structures for Processing with SIMD Instructions](https://openproceedings.org/2014/conf/edbt/ZeuchFH14.pdf)
1. [Compressed Computation for Text Indexing](http://www2.compute.dtu.dk/~npre/documents/thesis.pdf)
1. [Databases from finite categories](http://lambda-the-ultimate.org/node/5435)
1. https://github.com/datproject/docs/blob/master/papers/dat-paper.pdf
1. [Algorithms for Data Streams](http://twiki.di.uniroma1.it/pub/BDC/WebHome/SurveyStreaming08-DemetrescuFinocchi.pdf)
1. [Datafun: a Functional Datalog](https://people.mpi-sws.org/~neelk/datafun.pdf)
1. [Enabling Signal Processing over Data Streams](https://www.cs.ox.ac.uk/files/9135/sigmod2017-trilldsp.pdf)
1. [Database Learning: Toward a Database that Becomes Smarter Every Time](https://arxiv.org/abs/1703.05468)
1. https://github.com/jarulraj/databaseology
1. [Write-Behind Logging](http://www.vldb.org/pvldb/vol10/p337-arulraj.pdf)
1. [HoTTSQL: Proving Query Rewrites with Univalent SQL Semantics](https://homes.cs.washington.edu/~chushumo/files/cosette_pldi17.pdf)
1. [PebblesDB: Building Key-Value Stores using Fragmented Log-Structured Merge Trees](http://www.cs.utexas.edu/~vijay/papers/sosp17-pebblesdb.pdf)
1. [DROP: Dimensionality Reduction Optimization for Time Series](https://arxiv.org/abs/1708.00183)
1. http://www.cs.yale.edu/homes/aspnes/papers/skip-graphs-journal.pdf
1. https://www.amazon.com/Designing-Data-Intensive-Applications-Reliable-Maintainable/dp/1449373321
1. https://www.amazon.com/Database-Reliability-Engineering-Designing-Operating/dp/1491925949
1. https://www.enterprisetimes.co.uk/2017/02/22/can-severalnines-improve-mongodb-security/
1. https://severalnines.com/
1. http://engineering.pivotal.io/post/trilogy-the-sql-testing-framework/
1. https://techcrunch.com/2016/08/19/mysql-founder-tries-a-new-software-licensing-model/
1. https://blog.datopia.io/2018/11/03/hitchhiker-tree/

