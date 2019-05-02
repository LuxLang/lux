(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/new-luxc #=(identity version)
  :description "A re-written compiler for Lux."
  :url ~repo
  :license {:name "Lux License v0.1"
            :url ~(str repo "/blob/master/license.txt")}
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :repositories [["releases" ~sonatype-releases]
                 ["snapshots" ~sonatype-snapshots]
                 ["bedatadriven" "https://nexus.bedatadriven.com/content/groups/public/"]
                 ["jitpack" "https://jitpack.io"]]
  :scm {:name "git"
        :url ~(str repo ".git")}

  :dependencies [;; JVM Bytecode
                 [org.ow2.asm/asm-all "5.0.3"]
                 ;; ;; Scheme
                 ;; [kawa-scheme/kawa-core "2.4"]
                 ]
  
  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program "program"
        :test "test/program"}
  )
