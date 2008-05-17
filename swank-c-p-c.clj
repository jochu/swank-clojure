(clojure/in-ns 'swank)

(defn completions [string package]
  (simple-completions string package))

(provide :swank-c-p-c)