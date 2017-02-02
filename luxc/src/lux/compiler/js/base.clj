(ns lux.compiler.js.base
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftuple |let |do return* return |case]]
                 [host :as &host])
            [lux.compiler.core :as &&]
            )
  (:import (jdk.nashorn.api.scripting NashornScriptEngineFactory
                                      NashornScriptEngine
                                      ScriptObjectMirror
                                      JSObject)
           (jdk.nashorn.internal.runtime Undefined)
           (java.io File
                    BufferedOutputStream
                    FileOutputStream))
  )

(deftuple
  ["interpreter"
   "buffer"])

(defn js-host []
  (&/T [;; "interpreter"
        (.getScriptEngine (new NashornScriptEngineFactory))
        ;; "buffer"
        &/$None
        ]))

(defn run-js! [^String js-code]
  (fn [compiler-state]
    (|let [^NashornScriptEngine interpreter (->> compiler-state (&/get$ &/$host) (&/get$ $interpreter))]
      (try (&/$Right (&/T [compiler-state
                           (.eval interpreter js-code)]))
        (catch Exception ex
          (&/$Left (str ex)))))))

(def ^:private lux-obj-class (Class/forName "[Ljava.lang.Object;"))

(defn ^:private _valueOf_ [value]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      value)))

(defn ^:private _slice_ [wrap-lux-obj value]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      (let [slice (java.util.Arrays/copyOfRange value (aget args 0) (alength value))]
        (wrap-lux-obj slice)))))

(defn ^:private _toString_ [obj]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      (&/adt->text obj)
      ;; (pr-str this)
      )))

(deftype LuxJsObject [obj]
  JSObject
  (isFunction [self] false)
  (getSlot [self idx]
    (let [value (aget obj idx)]
      (if (instance? lux-obj-class value)
        (new LuxJsObject value)
        value)))
  (getMember [self member]
    (condp = member
      ;; "valueOf" (_valueOf_ obj)
      "toString" (_toString_ obj)
      "length" (alength obj)
      "slice" (let [wrap-lux-obj #(if (instance? lux-obj-class %)
                                    (new LuxJsObject %)
                                    %)]
                (_slice_ wrap-lux-obj obj))
      ;; else
      (assert false (str "wrap-lux-obj#getMember = " member)))))

(defn wrap-lux-obj [obj]
  (if (instance? lux-obj-class obj)
    (new LuxJsObject obj)
    obj))

(defn js-to-lux [js-object]
  (cond (or (nil? js-object)
            (instance? java.lang.Boolean js-object)
            (instance? java.lang.Integer js-object)
            (instance? java.lang.String js-object))
        js-object

        (instance? LuxJsObject js-object)
        (.-obj ^LuxJsObject js-object)

        ;; (instance? Undefined js-object)
        ;; (assert false "UNDEFINED")

        (instance? ScriptObjectMirror js-object)
        (let [^ScriptObjectMirror js-object js-object]
          (cond (.isArray js-object)
                (let [array-vec (loop [num-keys (.size js-object)
                                       idx 0
                                       array-vec []]
                                  (if (< idx num-keys)
                                    (let [idx-key (str idx)]
                                      (if (.hasMember js-object idx-key)
                                        (recur num-keys
                                               (inc idx)
                                               (conj array-vec (js-to-lux (.getMember js-object idx-key))))
                                        (recur (inc num-keys)
                                               (inc idx)
                                               (conj array-vec nil))))
                                    array-vec))]
                  (&/T array-vec))

                (.isFunction js-object)
                js-object

                :else
                (assert false (str "Unknown kind of JS object: " js-object))))

        :else
        (assert false (str "Unknown kind of JS object: " (class js-object) " :: " js-object))))

(defn run-js!+ [^String js-code]
  (|do [raw (run-js! js-code)]
    (return (js-to-lux raw))))

(def ^String unit (pr-str &/unit-tag))

(defn save-js! [name ^String script]
  (|do [_ (run-js! script)
        eval? &/get-eval
        module &/get-module-name
        :let [_ (when (not eval?)
                  (let [^String module* (&host/->module-class module)
                        module-dir (str @&&/!output-dir java.io.File/separator (.replace module* "/" java.io.File/separator))]
                    (do (.mkdirs (File. module-dir))
                      (&&/write-file (str module-dir java.io.File/separator name ".js") (.getBytes script)))))]]
    (return nil)))
