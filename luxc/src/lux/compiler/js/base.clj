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
  (&/$Js (&/T [;; "interpreter"
               (.getScriptEngine (new NashornScriptEngineFactory))
               ;; "buffer"
               &/$None
               ])))

(def ^String module-js-name "module.js")

(defn init-buffer []
  (&/change-js-host-slot $buffer (fn [_] (&/$Some (new StringBuilder)))))

(def get-buffer
  (|do [host &/js-host]
    (|case (&/get$ $buffer host)
      (&/$Some _buffer)
      (return _buffer)

      (&/$None)
      (&/fail-with-loc "[Error] No buffer available."))))

(defn run-js! [^String js-code]
  (|do [host &/js-host
        :let [interpreter ^NashornScriptEngine (&/get$ $interpreter host)]]
    (try (return (.eval interpreter js-code))
      (catch Exception ex
        (&/fail-with-loc (str ex))))))

(def ^:private lux-obj-class (Class/forName "[Ljava.lang.Object;"))

(defn ^:private _slice_ [wrap-lux-obj ^"[Ljava.lang.Object;" value]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      (let [slice (java.util.Arrays/copyOfRange value ^int (aget args 0) ^int (alength value))]
        (wrap-lux-obj slice)))))

(defn ^:private _toString_ [obj]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      (&/adt->text obj)
      )))

(defn ^:private _toString_simple [^String obj]
  (reify JSObject
    (isFunction [self] true)
    (call [self this args]
      obj
      )))

(def ^:private i64-mask (dec (bit-shift-left 1 32)))
(deftype I64 [value]
  JSObject
  (getMember [self member]
    (condp = member
      "H" (-> value (unsigned-bit-shift-right 32) (bit-and i64-mask) int)
      "L" (-> value (bit-and i64-mask) int)
      ;; else
      (assert false (str "I64#getMember = " member)))))

(defn ^:private encode-char [value]
  (reify JSObject
    (getMember [self member]
      (condp = member
        "C" value
        ;; "toString" (_toString_simple value)
        ;; else
        (assert false (str "encode-char#getMember = " member))))))

(deftype LuxJsObject [^"[Ljava.lang.Object;" obj]
  JSObject
  (isFunction [self] false)
  (getSlot [self idx]
    (let [value (aget obj idx)]
      (cond (instance? lux-obj-class value)
            (new LuxJsObject value)

            (instance? java.lang.Long value)
            (new I64 value)

            (instance? java.lang.Character value)
            (encode-char (str value))

            :else
            value)))
  (getMember [self member]
    (condp = member
      "toString" (_toString_ obj)
      "length" (alength obj)
      "slice" (_slice_ #(new LuxJsObject %) obj)
      ;; else
      (assert false (str "wrap-lux-obj#getMember = " member)))))

(defn wrap-lux-obj [obj]
  (if (instance? lux-obj-class obj)
    (new LuxJsObject obj)
    obj))

(defn ^:private int64? [^ScriptObjectMirror js-object]
  (and (.hasMember js-object "H")
       (.hasMember js-object "L")))

(defn ^:private encoded-char? [^ScriptObjectMirror js-object]
  (.hasMember js-object "C"))

(defn ^:private decode-char [^ScriptObjectMirror js-object]
  (-> ^String (.getMember js-object "C")
      (.charAt 0)))

(defn ^:private parse-int64 [^ScriptObjectMirror js-object]
  (+ (-> (.getMember js-object "H")
         long
         (bit-shift-left 32))
     (-> (.getMember js-object "L")
         long)))

(defn js-to-lux [js-object]
  (cond (or (nil? js-object)
            (instance? java.lang.Boolean js-object)
            (instance? java.lang.Integer js-object)
            (instance? java.lang.String js-object))
        js-object

        (instance? java.lang.Number js-object)
        (double js-object)

        (instance? LuxJsObject js-object)
        (.-obj ^LuxJsObject js-object)

        (instance? I64 js-object)
        (.-value ^I64 js-object)

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

                (int64? js-object)
                (parse-int64 js-object)

                (encoded-char? js-object)
                (decode-char js-object)

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
        ^StringBuilder buffer get-buffer
        :let [_ (when (not eval?)
                  (.append buffer ^String (str script "\n")))]]
    (return nil)))

(def save-module-js!
  (|do [eval? &/get-eval
        module &/get-module-name
        ^StringBuilder buffer get-buffer
        :let [_ (when (not eval?)
                  (let [^String module* (&host/->module-class module)
                        module-dir (str @&&/!output-dir java.io.File/separator (.replace module* "/" java.io.File/separator))]
                    (do (.mkdirs (File. module-dir))
                      (&&/write-file (str module-dir java.io.File/separator module-js-name)
                                     (.getBytes (.toString buffer))))))]]
    (return nil)))

(defn js-module [module]
  (string/replace module "/" "$"))

(defn js-var-name [module name]
  (str (js-module module) "$" (&host/def-name name)))
