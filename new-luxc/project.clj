(defproject com.github.luxlang/new-luxc "0.6.0-SNAPSHOT"
  :description "A re-written compiler for Lux."
  :url "https://github.com/LuxLang/lux"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :plugins [[com.github.luxlang/lein-luxc "0.6.0-SNAPSHOT"]]
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                        ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]]
  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}

  :dependencies [[org.ow2.asm/asm-all "5.0.3"]
                 [net.sandius.rembulan/rembulan-runtime "0.1-SNAPSHOT"]
                 [net.sandius.rembulan/rembulan-stdlib "0.1-SNAPSHOT"]
                 [net.sandius.rembulan/rembulan-compiler "0.1-SNAPSHOT"]
                 [org.jruby/jruby-complete "9.1.16.0"]
                 [org.python/jython-standalone "2.7.1"]]
  
  :source-paths ["source"]
  :test-paths ["test"]
  :lux {:program {:jvm "program"}
        :tests {:jvm "tests"}}
  )
