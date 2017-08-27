(defproject luxdoc "0.5.0"
  :description "Documentation generator for the Lux programming language's ."
  :url "https://github.com/LuxLang/luxdoc"
  :license {:name "Mozilla Public License (Version 2.0)"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :plugins [[com.github.luxlang/lein-luxc "0.5.0"]]
  :dependencies []
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots/"]
                 ["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2/"]]
  :source-paths ["source"]
  :lux {:program "program"})
