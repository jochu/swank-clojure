(ns swank.clj-contrib.pprint)

(def
 #^{:private true}
 pprint-enabled?
     (try (.loadClass (clojure.lang.RT/baseLoader) "clojure.contrib.pprint.PrettyWriter") (catch Exception e nil)))

(when pprint-enabled?
  (use 'clojure.contrib.pprint))

(defmacro pretty-pr-code*
  ([code]
     (if pprint-enabled?
       `(binding [*print-suppress-namespaces* true]
          (with-pprint-dispatch *code-dispatch* (write ~code :pretty true :stream nil)))
       `(pr-str ~code)))
  {:private true})

(defn pretty-pr-code [code]
  (pretty-pr-code* code))