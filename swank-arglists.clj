(clojure/in-ns 'swank)
(swank-require :swank-c-p-c)

(defn arglist-for-echo-area [raw-specs & options]
  (let [{:keys [arg-indices
                print-right-margin
                print-lines]} (apply hash-map options)]
    ;; Yeah, I'm lazy -- I'll flesh this out later
    (if (and (seq? raw-specs)
          (seq? (first raw-specs)))
      (operator-arglist (ffirst raw-specs) *buffer-package*)
      "")))

(defn variable-desc-for-echo-area [variable-name]
  (with-buffer-syntax
   (let [sym (resolve (symbol variable-name))]
     (when (. sym isBound)
       (str variable-name " => " (var-get sym))))))

(provide :swank-arglists)