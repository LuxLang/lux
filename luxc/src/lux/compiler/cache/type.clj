(ns lux.compiler.cache.type
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]]
                 [type :as &type])))

(def ^:private stop (->> 7 char str))
(def ^:private cons-signal (->> 5 char str))
(def ^:private nil-signal (->> 6 char str))

(defn ^:private serialize-list [serialize-type params]
  (str (&/fold (fn [so-far param]
                 (str so-far cons-signal (serialize-type param)))
               ""
               params)
       nil-signal))

(defn serialize-type
  "(-> Type Text)"
  [type]
  (if (clojure.lang.Util/identical &type/Type type)
    "T"
    (|case type
      (&/$Primitive name params)
      (str "^" name stop (serialize-list serialize-type params))

      (&/$Void)
      "0"

      (&/$Unit)
      "1"
      
      (&/$Product left right)
      (str "*" (serialize-type left) (serialize-type right))

      (&/$Sum left right)
      (str "+" (serialize-type left) (serialize-type right))

      (&/$Function left right)
      (str ">" (serialize-type left) (serialize-type right))

      (&/$UnivQ env body)
      (str "U" (serialize-list serialize-type env) (serialize-type body))

      (&/$ExQ env body)
      (str "E" (serialize-list serialize-type env) (serialize-type body))

      (&/$Bound idx)
      (str "$" idx stop)

      (&/$Ex idx)
      (str "!" idx stop)

      (&/$Var idx)
      (str "?" idx stop)

      (&/$Apply left right)
      (str "%" (serialize-type left) (serialize-type right))

      (&/$Named [module name] type*)
      (str "@" module &/+name-separator+ name stop (serialize-type type*))

      _
      (assert false (prn 'serialize-type (&type/show-type type)))
      )))

(declare deserialize-type)

(defn ^:private deserialize-list [^String input]
  (cond (.startsWith input nil-signal)
        [&/$Nil (.substring input 1)]

        (.startsWith input cons-signal)
        (when-let [[head ^String input*] (deserialize-type (.substring input 1))]
          (when-let [[tail ^String input*] (deserialize-list input*)]
            [(&/$Cons head tail) input*]))
        ))

(do-template [<name> <signal> <type>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      [<type> (.substring input 1)]
      ))

  ^:private deserialize-void  "0" &/$Void
  ^:private deserialize-unit  "1" &/$Unit
  ^:private deserialize-type* "T" &type/Type
  )

(do-template [<name> <signal> <type>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (when-let [[left ^String input*] (deserialize-type (.substring input 1))]
        (when-let [[right ^String input*] (deserialize-type input*)]
          [(<type> left right) input*]))
      ))

  ^:private deserialize-sum  "+" &/$Sum
  ^:private deserialize-prod "*" &/$Product
  ^:private deserialize-lambda    ">" &/$Function
  ^:private deserialize-app  "%" &/$Apply
  )

(do-template [<name> <signal> <type>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (let [[idx ^String input*] (.split (.substring input 1) stop 2)]
        [(<type> (Long/parseLong idx)) input*])))

  ^:private deserialize-bound "$" &/$Bound
  ^:private deserialize-ex    "!" &/$Ex
  ^:private deserialize-var   "?" &/$Var
  )

(defn ^:private deserialize-named [^String input]
  (when (.startsWith input "@")
    (let [[^String module+name ^String input*] (.split (.substring input 1) stop 2)
          [module name] (.split module+name "\\." 2)]
      (when-let [[type* ^String input*] (deserialize-type input*)]
        [(&/$Named (&/T [module name]) type*) input*]))))

(do-template [<name> <signal> <type>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (when-let [[env ^String input*] (deserialize-list (.substring input 1))]
        (when-let [[body ^String input*] (deserialize-type input*)]
          [(<type> env body) input*]))))

  ^:private deserialize-univq "U" &/$UnivQ
  ^:private deserialize-exq   "E" &/$ExQ
  )

(defn ^:private deserialize-host [^String input]
  (when (.startsWith input "^")
    (let [[name ^String input*] (.split (.substring input 1) stop 2)]
      (when-let [[params ^String input*] (deserialize-list input*)]
        [(&/$Primitive name params) input*]))))

(defn deserialize-type
  "(-> Text Type)"
  [input]
  (or (deserialize-type* input)
      (deserialize-void input)
      (deserialize-unit input)
      (deserialize-sum input)
      (deserialize-prod input)
      (deserialize-lambda input)
      (deserialize-app input)
      (deserialize-bound input)
      (deserialize-ex input)
      (deserialize-var input)
      (deserialize-named input)
      (deserialize-univq input)
      (deserialize-exq input)
      (deserialize-host input)
      (assert false (str "[Cache error] Cannot deserialize type. --- " input))))
