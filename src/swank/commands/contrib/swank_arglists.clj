(ns swank.commands.contrib.swank-arglists
  (:use (swank util core commands)))

((slime-fn 'swank-require) :swank-c-p-c)

(defslimefn arglist-for-echo-area [raw-specs & options]
  (let [{:keys [arg-indices
                print-right-margin
                print-lines]} (apply hash-map options)]
    ;; Yeah, I'm lazy -- I'll flesh this out later
    (if (and raw-specs
             (seq? raw-specs)
             (seq? (first raw-specs)))
      ((slime-fn 'operator-arglist) (ffirst raw-specs) *current-package*)
      nil)))

(defslimefn variable-desc-for-echo-area [variable-name]
  (with-emacs-package
   (or 
    (try
     (when-let [sym (read-string variable-name)]
       (when-let [var (resolve sym)]
         (when (.isBound #^clojure.lang.Var var)
           (str variable-name " => " (var-get var)))))
     (catch Exception e nil))
    "")))
