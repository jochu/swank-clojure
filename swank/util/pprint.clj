(ns swank.util.pprint)

(def pprint-enabled?
     (try (.loadClass (clojure.lang.RT/baseLoader) "clojure.contrib.pprint.PrettyWriter") (catch Exception e nil)))

(when pprint-enabled?
  (use 'clojure.contrib.pprint))

(defmacro pretty-pr-code [code]
  (if pprint-enabled?
    `(with-pprint-dispatch *code-dispatch* (write ~code :pretty true :stream nil))
    `(pr-str ~code)))
