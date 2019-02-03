(defproject com.github.luxlang/stdlib "0.6.0-SNAPSHOT"
  :description "Standard library for the Lux programming language."
  :url "https://github.com/LuxLang/stdlib"
  :license {:name "Lux License v0.1"
            :url "https://github.com/LuxLang/lux/blob/master/license.txt"}
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

  :dependencies []
  
  :source-paths ["source"]
  :test-paths ["test"]
  :lux {:tests {:jvm "test"}}
  )
