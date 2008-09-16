(ns swank.commands.contrib
  (:use (swank util core commands)))

(defslimefn swank-require [keys]
  (binding [*ns* (find-ns 'swank.commands.contrib)]
    (doseq k (if (seq? keys) keys (list keys))
      (try
       (load (str (name k) ".clj"))
       (catch java.io.FileNotFoundException fne nil)))))