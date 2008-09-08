(in-ns 'swank)

(defn completions [string package]
  (simple-completions string package))