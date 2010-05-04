(ns swank.clj-contrib.pprint)

(def
 #^{:private true}
 pprint-enabled?
 (try
  ;; 1.0, 1.1
  (do
    (.loadClass (clojure.lang.RT/baseLoader) "clojure.contrib.pprint.PrettyWriter")
    (use 'clojure.contrib.pprint)
    (defmacro pretty-pr-code*
      ([code]
         (if pprint-enabled?
           `(binding [*print-suppress-namespaces* true]
              (with-pprint-dispatch *code-dispatch* (write ~code :pretty true :stream nil)))
           `(pr-str ~code)))
      {:private true})
    true)
  (catch Exception e
    (try
     ;; 1.2
     (do
       (.getResource (clojure.lang.RT/baseLoader) "clojure/pprint")
       (use 'clojure.pprint)
       (defmacro pretty-pr-code*
         ([code]
            (if pprint-enabled?
              `(binding [*print-suppress-namespaces* true]
                 (with-pprint-dispatch code-dispatch (write ~code :pretty true :stream nil)))
              `(pr-str ~code)))
         {:private true})
       true)
     (catch Exception e
       (println e))))))

(defn pretty-pr-code [code]
  (pretty-pr-code* code))