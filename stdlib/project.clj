(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonetype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonetype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(defproject com.github.luxlang/stdlib #=(identity version)
  :description "Standard library for the Lux programming language."
  :url ~repo
  :license {:name "Lux License v0.1"
            :url ~(str repo "/blob/master/license.txt")}
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :deploy-repositories [["releases" {:url ~sonetype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonetype-snapshots :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :repositories [["releases" ~sonetype-releases]
                 ["snapshots" ~sonetype-snapshots]]
  :scm {:name "git"
        :url ~(str repo ".git")}

  :source-paths ["source"]
  :profiles {:library {:dependencies []
                       :lux {:tests {:jvm "test/lux"}}}
             :scriptum {:dependencies []
                        :lux {:program {:jvm "program/scriptum"}}}
             :licentia {:dependencies []
                        :lux {:program {:jvm "program/licentia"}
                              :tests {:jvm "test/licentia"}}}}
  )
