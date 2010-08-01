(ns leiningen.swank
  (:use [leiningen.compile :only [eval-in-project]])
  (:import [java.io File]))

(defn swank
  "Launch swank server for Emacs to connect. Optionally takes PORT and HOST."
  ([project port host & opts]
     (eval-in-project project
                      `(do
                         (let [is# ~(:init-script project)]
                           (when (and is# (.exists (File. is#)))
                             (load-file is#)))
                         (require '~'swank.swank)
                           (@(ns-resolve '~'swank.swank '~'start-repl)
                            (Integer. ~port)
                            ~@(concat (map read-string opts)
                                      [:host host])))))
  ([project port] (swank project port "localhost"))
  ([project] (swank project 4005)))
