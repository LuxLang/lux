# Multiversion Concurrency Control

0. []()
0. [Multiversion Concurrency Control: Theory and Algorithms](http://sungsoo.github.io/papers/bernstein-1983.pdf)

# Pagination

0. []()
0. [Pagination with Relative Cursors](https://shopify.engineering/pagination-relative-cursors)

# Reference

0. []()
0. [The Slotted Counter Pattern](https://planetscale.com/blog/the-slotted-counter-pattern)
0. ["Temporal Databases for Streaming Architectures" by Jeremy Taylor and Jon Pither](https://www.youtube.com/watch?v=ykbYNBE-V3k)
0. ["Makings of a Modern ACID Compliant Distributed Database" by Attila Szegedi](https://www.youtube.com/watch?v=pn8mCeX3LDE)
0. https://edgedb.com/blog/a-path-to-a-10x-database/
0. https://cstack.github.io/db_tutorial/
0. http://www.sql-workbench.eu/dbms_comparison.html
0. http://www.interdb.jp/pg/index.html
0. [Foundations of Databases](http://webdam.inria.fr/Alice/)
0. https://medium.com/textileio/building-the-firebase-for-crdts-7dd8dea8953a

# Data structure

0. []()
0. [The Concurrent 2-Trie](https://medium.com/@chrisvest/the-concurrent-2-trie-67deb2b57ba1)

# Query

0. []()
0. ["Morel, a functional query language" by Julian Hyde](https://www.youtube.com/watch?v=smUm3C5q2IM)
0. [Comprehending Ringads](http://lambda-the-ultimate.org/node/5525)
0. [Reasons why SELECT * is bad for SQL performance](https://tanelpoder.com/posts/reasons-why-select-star-is-bad-for-sql-performance/)
0. [A Short Story About SQL’s Biggest Rival](https://www.holistics.io/blog/quel-vs-sql/)
0. https://calcite.apache.org/
0. https://juxt.pro/blog/crux-sql
0. https://www.influxdata.com/blog/why-were-building-flux-a-new-data-scripting-and-query-language/
0. https://crate.io/a/lab-notes-how-we-made-joins-23-thousand-times-faster-part-two/
0. GC: A Graph Caching System for Subgraph/Supergraph Queries

	http://www.vldb.org/pvldb/vol11/p2022-wang.pdf
0. How to Architect a Query Compiler, Revisited

	https://www.cs.purdue.edu/homes/rompf/papers/tahboub-sigmod18.pdf
0. https://gql.today/
0. Everything You Always Wanted to Know About Compiled and Vectorized Queries But Were Afraid to Ask

	http://www.vldb.org/pvldb/vol11/p2209-kersten.pdf
0. https://github.com/efficient/SuRF
0. http://www.try-alf.org/blog/2013-10-21-relations-as-first-class-citizen
0. https://icfp18.sigplan.org/event/hope-2018-papers-finding-fixed-points-faster
0. Froid: Optimization of Imperative Programs in a Relational Database

	http://www.vldb.org/pvldb/vol11/p432-ramachandra.pdf
0. Query Combinators

	https://arxiv.org/abs/1702.08409
0. https://github.com/medmain/deepr
0. https://edgedb.com/blog/we-can-do-better-than-sql/
0. https://github.com/ollef/sixten/blob/master/docs/QueryCompilerDriver.md

# Optimization

0. []()
0. [Hera - High Efficiency Reliable Access to data stores](https://github.com/paypal/hera)
0. [Relaxed Operator Fusion for In-Memory Databases: Making Compilation, Vectorization, and Prefetching Work Together At Last](https://db.cs.cmu.edu/papers/2017/p1-menon.pdf)
0. https://medium.com/connect-the-dots/optimizing-queries-in-rethinkdb-584d7f660cb
0. https://blog.acolyer.org/2019/01/18/towards-a-hands-free-query-optimizer-through-deep-learning/
0. http://blog.felipe.rs/2019/01/29/demystifying-join-algorithms/
0. https://corecursive.com/030-rethinking-databases-with-jon-gjengset/

# Index

0. []()
0. https://www.pilosa.com/
0. https://en.wikipedia.org/wiki/Fractal_tree_index
0. [Beating hash tables with trees? The ART-ful radix trie](https://www.the-paper-trail.org/post/art-paper-notes/)
0. https://www.ristret.com/s/gnd4yr/brief_history_log_structured_merge_trees
0. [Getting The Most Out Of Your PostgreSQL Indexes](https://pgdash.io/blog/postgres-indexes.html?p)
0. http://databasearchitects.blogspot.com/2019/05/why-use-learning-when-you-can-fit.html
0. https://priyankvex.wordpress.com/2019/05/19/a-tale-on-concatenated-indexes-master-roshi-and-gokus-fireside-chat/

# Relational Algebra

0. []()
0. https://slamdata.com/what-is-multidimensional-relational-algebra/
0. https://github.com/slamdata/purescript-mra

# Time traveling

0. []()
0. https://www.arangodb.com/2018/07/time-traveling-with-graph-databases/

# Data processing

0. []()
0. https://medium.com/@gauthierleonard/apache-beam-a-unified-programming-model-for-data-processing-pipelines-4de9fb6ede6d

# Philosophy

0. []()
0. http://dhickey.ie/2016/01/03/commercial-suicide-integration-at-the-database-level/

# Storage

0. []()
0. [Database Internals: A deep-dive into how distributed data systems work](https://www.oreilly.com/library/view/database-internals/9781492040330/)
0. [B-Trees: More Than I Thought I'd Want to Know](https://benjamincongdon.me/blog/2021/08/17/B-Trees-More-Than-I-Thought-Id-Want-to-Know/)
0. [Understanding LSM Trees: What Powers Write-Heavy Databases](https://yetanotherdevblog.com/lsm/)
0. http://www.benstopford.com/2015/02/14/log-structured-merge-trees/
0. A Comparison of Adaptive Radix Trees and Hash Tables

	https://bigdata.uni-saarland.de/publications/ARCD15.pdf
0. HashKV: Enabling Efficient Updates in KV Storage via Hashing

	https://www.usenix.org/system/files/conference/atc18/atc18-chan.pdf
0. https://akumuli.org/akumuli/2018/08/03/high-cardinality-support/
0. Ideal Hash Trees

	https://github.com/papers-we-love/papers-we-love/blob/master/data_structures/ideal-hash-trees.pdf
0. https://clemenswinter.com/2018/08/13/how-read-100s-of-millions-of-records-per-second-from-a-single-disk/
0. https://github.com/vmxdev/tkvdb
0. https://blog.acolyer.org/2018/09/28/columnstore-and-b-tree-are-hybrid-physical-designs-important/
0. http://sergeiturukin.com/2017/06/07/storage-engine-introduction.html
0. https://accumulo.apache.org/
0. http://www.mit.edu/~kepner/D4M/
0. Achieving 100,000,000 database inserts per second using Accumulo and D4M

	http://www.ieee-hpec.org/2014/CD/index_htm_files/FinalPapers/31.pdf
0. Lambda World 2018 - The Radix Trees How IntMap Works - Tikhon Jelvis

	https://www.youtube.com/watch?v=0udjkEiCjog
0. FineLine: Log-structured Transactional Storage and Recovery

	http://www.vldb.org/pvldb/vol11/p2249-sauer.pdf
0. Storm: a fast transactional dataplane forremote data structures

	http://www.cs.technion.ac.il/~dan/papers/storm-systor-2019.pdf

# Exemplar

0. []()
0. [Debunking “Purpose-Built Data Systems”: Enter the Universal Database](https://tiledb.com/blog/debunking-purpose-built-data-systems-enter-the-universal-database-2021-08-04)
0. [Irmin](https://irmin.org/)
0. [Database of Databases](https://dbdb.io/)

## General

0. [FeatureBase: The First OLAP Database Built Entirely on Bitmaps](https://www.featurebase.com/)
	0. [FeatureBase](https://github.com/FeatureBaseDB/featurebase)
0. [Velox: Meta’s Unified Execution Engine](https://research.facebook.com/publications/velox-metas-unified-execution-engine/)
0. [Skytable](https://skytable.io/)
0. https://github.com/Workiva/eva/
0. https://learndb.net/
0. http://orientdb.com/
0. https://ignite.apache.org/
0. https://eventsourcing.com/
0. https://geteventstore.com/
0. http://cassandra.apache.org/
0. https://kafka.apache.org/
0. http://basho.com/products/riak-kv/
0. https://www.influxdata.com/time-series-platform/influxdb/
0. https://prestodb.io/
0. https://github.com/mozilla/datomish
0. http://sqream.com/
0. http://pelotondb.io/
0. https://github.com/tidwall/summitdb
0. https://github.com/facebookincubator/beringei

	https://code.facebook.com/posts/952820474848503/beringei-a-high-performance-time-series-storage-engine
0. https://github.com/tonsky/datascript
0. https://github.com/biokoda/actordb
0. https://www.slashdb.com/
0. https://realm.io/
0. https://www.bigchaindb.com/
0. http://samza.apache.org/
0. https://www.umiacs.umd.edu/~zhangyp/papers/IntegriDB.pdf
0. https://github.com/PumpkinDB/PumpkinDB
0. https://symas.com/offerings/lightning-memory-mapped-database/
0. http://gun.js.org/
0. https://open.dgraph.io/post/benchmark-neo4j/

	0. https://github.com/dgraph-io/dgraph
	0. https://dgraph.io/
0. https://www.spreadsheetdb.io/
0. http://www.openio.io/
0. https://github.com/pingcap/tidb
0. http://druid.io/
0. http://hsqldb.org/
0. http://siridb.net/blog/time-series-with-siridb/
0. https://github.com/jankotek/mapdb
0. https://github.com/NationalSecurityAgency/lemongraph
0. https://twobithistory.org/2017/10/07/the-most-important-database.html
0. https://github.com/couchbase/forestdb
0. https://www.nuodb.com/
0. https://edgedb.com/
0. https://www.harperdb.io/
0. https://www.antidotedb.eu/
0. https://tech.marksblogg.com/minimalist-guide-tutorial-foundationdb.html
0. https://www.foundationdb.org/blog/announcing-document-layer/
0. https://www.foundationdb.org/
	0. https://en.wikipedia.org/wiki/FoundationDB
0. https://github.com/orbitdb/orbit-db
0. https://www.aerospike.com/
0. https://sirix.io/

## Relational

0. []()
0. [dqlite: Embeddable, replicated and fault tolerant SQL engine.](https://github.com/canonical/dqlite)

## Immutable

0. []()
0. [immudb: OPEN SOURCE IMMUTABLE DATABASE](https://codenotary.com/technologies/immudb/)
0. http://www.datomic.com/

## Data-flow

0. []()
0. [Noria: data-flow for high-performance web applications](https://github.com/mit-pdos/noria)

## Vector Database

0. []()
0. [What is a Vector Database?](https://www.pinecone.io/learn/vector-database/)

## Graph

0. []()
0. ["Asami: Turn your JSON into a Graph in 2 Lines" by Paula Gearon](https://www.youtube.com/watch?v=-XegX_K6w-o)
0. https://github.com/hugegraph/hugegraph
0. https://github.com/Microsoft/GraphEngine
0. https://redislabs.com/blog/release-redisgraph-v1-0-preview/
0. https://github.com/JanusGraph/janusgraph
0. https://www.slideshare.net/RoiLipman/graph-algebra
0. https://juxt.pro/crux/docs/index.html
0. https://github.com/juxt/crux
0. [The Crux of Bitemporality - Jon Pither](https://www.youtube.com/watch?v=3Stja6YUB94)
0. [Bitemporality: More Than a Design Pattern](https://juxt.pro/blog/bitemporality-more-than-a-design-pattern)

## Column

0. []()
0. https://blog.acolyer.org/2018/09/26/the-design-and-implementation-of-modern-column-oriented-database-systems/

## Datalog

0. []()
0. [Open Source Clojure-Datalog Databases](https://clojurelog.github.io/)

## Log

0. []()
0. https://code.fb.com/core-data/logdevice-a-distributed-data-store-for-logs/

## Key-Value

0. []()
0. https://github.com/yahoo/HaloDB
0. https://chronicle.software/products/map/

## Array

0. []()
0. [Array Databases: Concepts, Standards, Implementations](https://rd-alliance.org/system/files/Array-Databases_final-report.pdf)

## Blockchain

0. []()
0. https://flur.ee/

## Time-series

0. []()
0. https://github.com/alpacahq/marketstore
0. https://www.outlyer.com/blog/why-not-to-build-a-time-series-database/

## GPU

0. []()
0. https://blazingdb.com/#/

## Tree

0. []()
0. A Novel Method for Representing Hierarchies in a Relational Database Using Bignums and SQLite

	https://s3.amazonaws.com/pub.xhuntley.net/Huntley_Tcl2011.pdf

# CQRS | Event-Sourcing

0. []()
0. [Introducing Derivative Event Sourcing](https://www.confluent.io/blog/event-sourcing-vs-derivative-event-sourcing-explained)
0. https://blog.wallaroolabs.com/2018/06/implementing-time-windowing-in-an-evented-streaming-system/
0. https://data-artisans.com/blog/a-practical-guide-to-broadcast-state-in-apache-flink
0. https://medium.com/@hugo.oliveira.rocha/what-they-dont-tell-you-about-event-sourcing-6afc23c69e9a
0. https://vvvvalvalval.github.io/posts/2018-11-12-datomic-event-sourcing-without-the-hassle.html
0. https://chriskiehl.com/article/event-sourcing-is-hard
0. https://medium.com/serialized-io/apache-kafka-is-not-for-event-sourcing-81735c3cf5c

# Architecture

0. []()
0. https://www.sqlite.org/arch.html

# Criticism

0. []()
0. https://codeburst.io/doing-without-databases-in-the-21st-century-6e25cf495373

# Geo-spatial

0. []()
0. H3: Uber’s Hexagonal Hierarchical Spatial Index

	https://eng.uber.com/h3/
0. http://s2geometry.io/
0. https://github.com/mourner/flatbush

# Transaction

0. []()
0. MixT: An embedded, domain-specific language for Mixed-Consistency Transactions

	https://mpmilano.github.io/MixT/
0. The Case for Shared Nothing

	http://db.cs.berkeley.edu/papers/hpts85-nothing.pdf

# Consistency

0. []()
0. https://jepsen.io/consistency
0. http://muratbuffalo.blogspot.com/2018/08/the-many-faces-of-consistency.html
0. https://fauna.com/blog/consistency-without-clocks-faunadb-transaction-protocol

# P2P Networking

0. []()
0. https://medium.com/perlin-network/noise-an-opinionated-p2p-networking-stack-for-decentralized-protocols-in-go-bfc6fecf157d
0. https://xorro-p2p.github.io/

# Data Visualization

0. []()
0. https://www.tadviewer.com/

# Privacy

0. []()
0. https://blog.acolyer.org/2019/06/17/towards-multiverse-databases/
0. [Towards Multiverse Databases](https://people.csail.mit.edu/malte/pub/papers/2019-hotos-multiversedb.pdf)

# Security

0. []()
0. [The ZombieLoad Pragmatist: Tips for Surviving in a Post-Meltdown World](https://www.scylladb.com/2019/05/17/the-zombieload-pragmatist/)

# _Temporary cache_

0. []()
0. https://github.com/pubkey/rxdb
0. https://hackernoon.com/execute-millions-of-sql-statements-in-milliseconds-in-the-browser-with-webassembly-and-web-workers-3e0b25c3f1a6#.w8w8qi8b3
0. https://github.com/kripken/sql.js
0. http://zmtp.org/
0. https://github.com/agentm/project-m36
0. http://graphql.org/
0. https://docs.microsoft.com/en-us/azure/documentdb/documentdb-introduction
0. http://storm.apache.org/
0. http://www.intersystems.com/our-products/cache/cache-overview/
0. https://prometheus.io/
0. http://www.ragic.com/
0. https://github.com/bittorrent/sqltorrent
0. https://github.com/bitnine-oss/agensgraph
0. [Comdb2: Bloomberg’s Highly Available Relational Database System](http://www.vldb.org/pvldb/vol9/p1377-scotti.pdf)
0. https://www.pipelinedb.com/
0. https://fauna.com/home
0. https://fauna.com/blog/a-database-built-like-an-operating-system
0. http://www.timescaledb.com/
0. https://www.dataengineeringpodcast.com/episode-4-scylladb-with-eyal-gutkind/
0. http://quickstep.incubator.apache.org/
0. https://barrel-db.org/
0. https://misfra.me/2016/04/09/tsdb-list/
0. https://github.com/cayleygraph/cayley
0. https://deepstreamhub.com/
0. http://www.spdk.io/
0. https://github.com/pilosa/pilosa
0. http://sophia.systems/
0. https://humio.com/
0. https://medium.com/@uwdb/introducing-cosette-527898504bd6
0. https://mixmax.com/blog/bee-queue-v1-node-redis-queue
0. http://btrdb.io/
0. https://clickhouse.yandex/
0. https://github.com/rqlite/rqlite
0. https://www.confluent.io/blog/ksql-open-source-streaming-sql-for-apache-kafka/
0. https://github.com/Tencent/paxosstore
0. https://www.ibm.com/us-en/marketplace/project-eventstore
0. [LogDevice: a distributed data store for logs](https://code.facebook.com/posts/357056558062811)
0. https://crate.io/
0. http://traildb.io/
0. http://profanedb.gitlab.io/
0. https://github.com/PyroclastIO/metamorphic
0. https://franchise.cloud/
0. https://github.com/ssbc/secure-scuttlebutt
0. ["Datafun: a functional query language" by Michael Arntzenius](https://www.youtube.com/watch?v=gC295d3V9gE)
0. https://github.com/neo4j-contrib/spatial
0. https://github.com/agershun/alasql
0. http://www.tiledb.io/
0. https://databricks.com/product/databricks-delta
0. http://www.geomesa.org/
0. https://bookkeeper.apache.org/
0. https://blog.outlyer.com/top10-open-source-time-series-databases
0. https://github.com/qubole/rubix
0. https://github.com/confluentinc/ksql
0. https://www.datanami.com/2018/06/15/streaming-sql-for-real-time-analytics/
0. https://blog.fauna.com/fauna-topology-operations
0. https://fabianlindfors.se/blog/building-an-object-store-with-foundation-db/
0. https://use-the-index-luke.com/no-offset
0. [GOTO 2016 • Conflict Resolution for Eventual Consistency • Martin Kleppmann](https://www.youtube.com/watch?v=yCcWpzY8dIA)
0. https://www.thoughts-on-java.org/5-common-hibernate-mistakes-that-cause-dozens-of-unexpected-queries/
0. https://www.nicolaferraro.me/2018/04/25/saga-pattern-in-apache-camel/
0. http://augustl.com/blog/2018/datomic_look_at_all_the_things_i_am_not_doing/
0. https://blog.2ndquadrant.com/application-users-vs-row-level-security/
0. https://medium.com/databasss/on-ways-to-agree-part-1-links-and-flp-impossibility-f6bd8a6a0980
0. https://medium.com/@robot_dreams/disk-can-be-faster-than-memory-37c57e7ebca7
0. http://www.bailis.org/blog/when-is-acid-acid-rarely/
0. https://hackernoon.com/five-data-models-for-sharding-and-which-is-right-f5c3fa480b00
0. http://www.aosabook.org/en/500L/an-archaeology-inspired-database.html
0. https://www.clever-cloud.com/blog/engineering/2015/05/20/why-auto-increment-is-a-terrible-idea/
0. https://samrat.me/posts/2017-11-04-kvstore-linear-hashing/
0. https://detect.io/news/2017/10/16/three-time-series-that-defeat-typical-anomaly-detection
0. http://blog.memsql.com/scaling-distributed-joins/
0. http://akumuli.org/akumuli/2017/11/17/indexing/
0. https://10clouds.com/blog/postgresql-10/
0. https://cstack.github.io/db_tutorial/
0. https://msdn.microsoft.com/en-us/library/jj591569.aspx
0. https://chatwithengineers.com/2016/08/29/a-survey-of-query-execution-engines-from-volcano-to-vectorized-processing/
0. https://martin.kleppmann.com/2015/05/11/please-stop-calling-databases-cp-or-ap.html
0. https://medium.com/the-hoard/building-a-kafka-that-doesnt-depend-on-zookeeper-2c4701b6e961#.paa4pjdtc
0. https://blog.acolyer.org/2017/01/23/ground-a-data-context-service/
0. https://engineering.instagram.com/sharding-ids-at-instagram-1cf5a71e5a5c#.omhi4kyaj
0. http://rystsov.info/2017/02/15/simple-consensus.html
0. https://blog.jooq.org/2016/10/05/why-you-should-design-your-database-to-optimise-for-statistics/
0. https://thesquareplanet.com/blog/students-guide-to-raft/
0. https://rjuju.github.io/postgresql/2015/07/02/how-about-hypothetical-indexes.html
0. http://amitkapila16.blogspot.com/2017/03/hash-indexes-are-faster-than-btree.html
0. http://rhaas.blogspot.com/2017/04/new-features-coming-in-postgresql-10.html
0. https://fabxc.org/blog/2017-04-10-writing-a-tsdb/#
0. https://github.com/confluentinc/bottledwater-pg
0. http://www.postgresonline.com/journal/archives/173-Using-LTree-to-Represent-and-Query-Hierarchy-and-Tree-Structures.html
0. https://juxt.pro/blog/posts/datascript-dom.html
0. https://github.com/agentm/project-m36
0. https://fortytw2.com/relational-dbs-on-kv-store-pt-1
0. https://blog.slicingdice.com/want-to-build-a-new-time-series-database-start-here-d4c3fd1517b1
0. http://akumuli.org/akumuli/2017/04/29/nbplustree/
0. https://www.hugopicado.com/2017/05/06/what-event-sourcing-is-not.html
0. https://news.realm.io/news/conflict-resolution-for-eventual-consistency-goto/
0. https://www.citusdata.com/blog/2017/05/11/dynamo-citus-and-tradeoffs-in-distributed-databases/
0. http://www.cakesolutions.net/teamblogs/how-to-build-a-distributed-counter
0. https://netsil.com/blog/comparison-of-time-series-databases-netsils-use-of-druid/
0. https://www.infoq.com/interviews/meijer-big-data
0. https://www.cs.cmu.edu/~jarulraj/pages/sigmod_2017_tutorial.html
0. https://speakerdeck.com/caitiem20/distributed-sagas-a-protocol-for-coordinating-microservices
0. https://blog.timescale.com/what-the-heck-is-time-series-data-and-why-do-i-need-a-time-series-database-dcf3b1b18563
0. https://medium.com/hyrise/the-brain-of-every-database-c622aaba7d75
0. http://codecapsule.com/2012/11/07/ikvs-implementing-a-key-value-store-table-of-contents/
0. https://www.cockroachlabs.com/blog/sql-in-cockroachdb-mapping-table-data-to-key-value-storage/
0. https://blog.acolyer.org/2017/07/07/do-we-need-specialized-graph-databases-benchmarking-real-time-social-networking-applications/
0. http://vvvvalvalval.github.io/posts/2017-07-08-Datomic-this-is-not-the-history-youre-looking-for.html?t=1&cn=ZmxleGlibGVfcmVjc18y&iid=1e725951a13247d8bdc6fe8c113647d5&uid=2418042062&nid=244+293670920
0. https://maxdemarzi.com/2017/07/13/using-a-cuckoo-filter-for-unique-relationships/
0. https://medium.baqend.com/real-time-databases-explained-why-meteor-rethinkdb-parse-and-firebase-dont-scale-822ff87d2f87
0. http://dawn.cs.stanford.edu/2017/08/07/asap/
0. [TimescaleDB vs. Postgres for time-series: 20x higher inserts, 2000x faster deletes, 1.2x-14,000x faster queries](https://blog.timescale.com/timescaledb-vs-6a696248104e)
0. https://www.elastic.co/blog/elasticsearch-sequence-ids-6-0
0. https://neo4j.com/blog/efficient-graph-algorithms-neo4j/
0. https://blog.jooq.org/2017/05/30/when-to-use-bind-values-and-when-to-use-inline-values-in-sql/
0. https://fylux.github.io/2017/08/09/VAR_Model_Time_Series/
0. https://brandur.org/postgres-atomicity
0. [Generative Programming and Verification - Nada Amin](https://www.youtube.com/watch?v=QuJ-cEvH_oI)
0. http://ithare.com/oltp-db-optimizations-101-thinking-in-terms-of-execution-plans/
0. https://pingcap.github.io/blog/2017/08/15/multi-raft/
0. https://medium.com/@ajstorm/the-lambda-architecture-simplified-a28e436fa55e
0. http://akumuli.org/akumuli/2016/12/30/compression_part1/
0. https://en.wikipedia.org/wiki/Codd%27s_12_rules
0. https://blog.jooq.org/2017/09/28/10-cool-sql-optimisations-that-do-not-depend-on-the-cost-model/
0. https://medium.com/@ifesdjeen/on-disk-storage-part-4-b-trees-30791060741
0. https://www.cockroachlabs.com/blog/automated-rebalance-and-repair/
0. http://danielwhittaker.me/2017/10/09/handle-set-based-consistency-validation-cqrs/
0. http://db.cs.cmu.edu/seminar2017/
0. https://blog.timescale.com/time-series-data-postgresql-10-vs-timescaledb-816ee808bac5
0. [Is Datomic strictly better than Facebook's graph datastore?](http://www.dustingetz.com/ezpjb2RlLWRhdGFiYXNlICJzdGFydGVyLWJsb2ctc3JjMiIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU1NTkgI1VSSSAiZGF0b21pYzpmcmVlOi8vZGF0b21pYzo0MzM0L3N0YXJ0ZXItYmxvZy1zcmMyIl0sIDpxdWVyeS1wYXJhbXMgezplbnRpdHkgI0RiSWRbMTc1OTIxODYwNDYxNDQgI1VSSSAiZGF0b21pYzpmcmVlOi8vZGF0b21pYzo0MzM0L2R1c3RpbmdldHouY29tIl19fQ,,)
0. https://www.citusdata.com/blog/2017/10/17/tour-of-postgres-index-types/
0. https://blog.grakn.ai/modelling-data-with-hypergraphs-edff1e12edf0
0. http://www.scylladb.com/2017/10/05/io-access-methods-scylla/
0. http://code.hootsuite.com/row-stores-vs-column-stores-why-not-use-both/
0. http://matthewrocklin.com/blog/work/2017/10/16/streaming-dataframes-1
0. https://brightinventions.pl/blog/database-timeouts/
0. https://brandur.org/redis-streams
0. https://www.fittedcloud.com/blog/machine-learning-driven-optimization-outperforms-auto-scaling-dynamodb-capacity-management/
0. https://parquet.apache.org/
0. https://arrow.apache.org/
0. https://www.youtube.com/watch?v=6lCVKMQR8Dw
0. https://blog.acolyer.org/2015/10/15/ironfleet-proving-practical-distributed-systems-correc/
0. https://underscore.io/blog/posts/2017/03/07/refined-data-config-database.html
0. https://www.quickdatabasediagrams.com/
0. [The Case for Learned Index Structures](https://arxiv.org/abs/1712.01208)
0. https://arxiv.org/abs/0806.3115
0. https://arxiv.org/abs/1604.03607
0. https://arxiv.org/abs/1702.07588
0. [HoTTSQL: Proving Query Rewrites with Univalent SQL Semantics](https://arxiv.org/abs/1607.04822)
0. http://www.vldb.org/pvldb/vol9/p1707-pirk.pdf
0. [Continuous Analytics Over Discontinuous Streams](https://people.eecs.berkeley.edu/~franklin/Papers/sigmod10krishnamurthy.pdf)
0. http://www.vldb.org/
0. https://blog.acolyer.org/2017/01/30/how-good-are-query-optimizers-really/
0. http://gun.js.org/distributed/matters.html
0. http://blog.datomic.com/2017/01/the-ten-rules-of-schema-growth.html
0. https://blogs.technet.microsoft.com/dataplatforminsider/2017/01/05/how-six-lines-of-code-sql-server-can-bring-deep-learning-to-any-app/
0. https://muratbuffalo.blogspot.com/2014/07/hybrid-logical-clocks.html
0. https://yokota.blog/2017/02/17/dont-settle-for-eventual-consistency/
0. [AN INTRODUCTION TO GRAPH COMPRESSION TECHNIQUES FOR IN-MEMORY GRAPH COMPUTATION](https://www.cs.umd.edu/sites/default/files/scholarly_papers/Chavan.pdf)
0. [Relational Lattices: From Databases to Universal Algebra](http://www.dcs.bbk.ac.uk/~szabolcs/rellat-jlamp-second-submission-2.pdf)
0. https://bitworking.org/news/2017/03/prometheus
0. [Partioned Elias-Fano indexes](http://hpc.isti.cnr.it/hpcworkshop2014/PartitionedEliasFanoIndexes.pdf)
0. http://h2co3.org/blog/index.php/2017/04/10/join-considered-harmful/
0. https://blog.jooq.org/2016/07/05/say-no-to-venn-diagrams-when-explaining-joins/
0. [A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing](http://www.vldb.org/pvldb/vol9/p96-richter.pdf)
0. https://research.googleblog.com/2017/04/consistent-hashing-with-bounded-loads.html
0. [The Adaptive Radix Tree: ARTful Indexing for Main-Memory Databases](https://pdfs.semanticscholar.org/6abf/5107efc723c655956f027b4a67565b048799.pdf)
0. [Adapting Tree Structures for Processing with SIMD Instructions](https://openproceedings.org/2014/conf/edbt/ZeuchFH14.pdf)
0. [Compressed Computation for Text Indexing](http://www2.compute.dtu.dk/~npre/documents/thesis.pdf)
0. [Databases from finite categories](http://lambda-the-ultimate.org/node/5435)
0. https://github.com/datproject/docs/blob/master/papers/dat-paper.pdf
0. [Algorithms for Data Streams](http://twiki.di.uniroma1.it/pub/BDC/WebHome/SurveyStreaming08-DemetrescuFinocchi.pdf)
0. [Datafun: a Functional Datalog](https://people.mpi-sws.org/~neelk/datafun.pdf)
0. [Enabling Signal Processing over Data Streams](https://www.cs.ox.ac.uk/files/9135/sigmod2017-trilldsp.pdf)
0. [Database Learning: Toward a Database that Becomes Smarter Every Time](https://arxiv.org/abs/1703.05468)
0. https://github.com/jarulraj/databaseology
0. [Write-Behind Logging](http://www.vldb.org/pvldb/vol10/p337-arulraj.pdf)
0. [HoTTSQL: Proving Query Rewrites with Univalent SQL Semantics](https://homes.cs.washington.edu/~chushumo/files/cosette_pldi17.pdf)
0. [PebblesDB: Building Key-Value Stores using Fragmented Log-Structured Merge Trees](http://www.cs.utexas.edu/~vijay/papers/sosp17-pebblesdb.pdf)
0. [DROP: Dimensionality Reduction Optimization for Time Series](https://arxiv.org/abs/1708.00183)
0. http://www.cs.yale.edu/homes/aspnes/papers/skip-graphs-journal.pdf
0. https://www.amazon.com/Designing-Data-Intensive-Applications-Reliable-Maintainable/dp/1449373321
0. https://www.amazon.com/Database-Reliability-Engineering-Designing-Operating/dp/1491925949
0. https://www.enterprisetimes.co.uk/2017/02/22/can-severalnines-improve-mongodb-security/
0. https://severalnines.com/
0. http://engineering.pivotal.io/post/trilogy-the-sql-testing-framework/
0. https://techcrunch.com/2016/08/19/mysql-founder-tries-a-new-software-licensing-model/
0. https://blog.datopia.io/2018/11/03/hitchhiker-tree/

