;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(def version "0.6.0-SNAPSHOT")
(def repo "https://github.com/LuxLang/lux")
(def sonatype-releases "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
(def sonatype-snapshots "https://oss.sonatype.org/content/repositories/snapshots/")

(def jphp-version "0.9.2")

(defproject com.github.luxlang/lux-php #=(identity version)
  :description "A PHP compiler for Lux."
  :url ~repo
  :license {:name "Lux License v0.1.1"
            :url ~(str repo "/blob/master/license.txt")}
  :scm {:name "git"
        :url ~(str repo ".git")}
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  
  :repositories [["releases" ~sonatype-releases]
                 ["snapshots" ~sonatype-snapshots]
                 ["jitpack" "https://jitpack.io"]]
  :deploy-repositories [["releases" {:url ~sonatype-releases :creds :gpg}]
                        ["snapshots" {:url ~sonatype-snapshots :creds :gpg}]]
  
  :plugins [[com.github.luxlang/lein-luxc ~version]]
  :dependencies [[com.github.luxlang/luxc-jvm ~version]
                 [com.github.luxlang/stdlib ~version]
                 ;; PHP 5
                 [org.develnext.jphp/jphp-core ~jphp-version]
                 [org.develnext.jphp/jphp-scripting ~jphp-version]]

  :manifest {"lux" ~version}
  :source-paths ["source"]
  :lux {:program "program"}
  )
