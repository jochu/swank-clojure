(in-ns 'swank)

(defn arglist-for-echo-area [raw-specs & options]
  (let [{:keys [arg-indices
                print-right-margin
                print-lines]} (apply hash-map options)]
    ;; Yeah, I'm lazy -- I'll flesh this out later
    (if (and raw-specs
             (seq? raw-specs)
             (seq? (first raw-specs)))
      (operator-arglist (ffirst raw-specs) *buffer-package*)
      nil)))

(defn variable-desc-for-echo-area [variable-name]
  (with-buffer-syntax
   (or
    (when-let sym (from-string variable-name)
      (when-let var (resolve sym)
        (when (. var isBound)
          (str variable-name " => " (var-get var)))))
    "")))