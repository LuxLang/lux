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
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]
                 ["bedatadriven" "https://nexus.bedatadriven.com/content/groups/public/"]
                 ["jitpack" "https://jitpack.io"]]
  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}

  :dependencies [;; JVM Bytecode
                 [org.ow2.asm/asm-all "5.0.3"]
                 ;; LUA
                 ;; [net.sandius.rembulan/rembulan-runtime "0.1-SNAPSHOT"]
                 ;; [net.sandius.rembulan/rembulan-stdlib "0.1-SNAPSHOT"]
                 ;; [net.sandius.rembulan/rembulan-compiler "0.1-SNAPSHOT"]
                 ;; Ruby
                 ;; [org.jruby/jruby-complete "9.1.16.0"]
                 ;; Python
                 ;; [org.python/jython-standalone "2.7.1"]
                 ;; R
                 ;; [org.renjin/renjin-script-engine "0.8.2527"]
                 ;; Scheme
                 ;; [kawa-scheme/kawa-core "2.4"]
                 ;; Common Lisp
                 ;; [org.abcl/abcl "1.5.0"]
                 ;; PHP 5
                 [org.develnext.jphp/jphp-core "0.9.2"]
                 [org.develnext.jphp/jphp-scripting "0.9.2"]]
  
  :source-paths ["source"]
  :test-paths ["test"]
  :lux {:program {:jvm "program"}
        :tests {:jvm "tests"}}
  )
