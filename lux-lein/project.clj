;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject com.github.luxlang/lein-luxc "0.5.0"
  :description "The Leiningen plugin for the Lux programming language."
  :url "https://github.com/LuxLang/lein-luxc"
  :license {:name "Mozilla Public License"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.github.luxlang/luxc-jvm "0.5.0"]
                 [com.github.luxlang/stdlib "0.5.0"]]
  :deploy-repositories [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                     :creds :gpg}]
                        ["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]]
  :pom-addition [:developers [:developer
                              [:name "Eduardo Julian"]
                              [:url "https://github.com/eduardoejp"]]]
  :scm {:name "git"
        :url "https://github.com/LuxLang/lux.git"}
  
  :eval-in :leiningen)
