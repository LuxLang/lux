;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns leiningen.lux.watch
  (:require [leiningen.core.classpath :as classpath])
  (:import (java.io File)
           (java.nio.file FileSystems
                          Path
                          WatchEvent$Kind
                          StandardWatchEventKinds
                          WatchService
                          WatchKey)))

(defn ^:private file-tree [path]
  (let [dir (new File path)]
    (if (and (.exists dir)
             (.isDirectory dir))
      (->> (.listFiles dir) (mapcat (comp file-tree #(.getAbsolutePath ^File %))) (cons path))
      (list))))

(defn ^:private drain! [^WatchService watcher]
  (when-let [^WatchKey key (.poll watcher)]
    (when (and (.isValid key)
               (not (.isEmpty (.pollEvents key))))
      
      (.reset key)
      (recur watcher))))

(defn watch [action project]
  (let [fs (FileSystems/getDefault)
        ^WatchService watcher (.newWatchService fs)
        dirs-to-watch (->> (concat (get project :test-paths (list))
                                   (get project :source-paths (list)))
                           (mapcat file-tree)
                           (map #(.getPath fs % (into-array String []))))
        _ (doseq [^Path dir dirs-to-watch]
            (.register dir watcher (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_MODIFY])))]
    (do (action)
      (loop []
        (do (when-let [^WatchKey key (.poll watcher)]
              (when (.isValid key)
                (.pollEvents key)
                (.reset key)
                (drain! watcher)
                (action)))
          (Thread/sleep 1000)
          (recur))))
    ))
