(ns leiningen.swank
  (:use [leiningen.compile :only [eval-in-project]])
  (:import [java.io File]))

(defn swank-form [project port host opts]
  `(do
     (let [is# ~(:repl-init-script project)]
       (when (.exists (File. (str is#)))
         (load-file is#)))
     (require '~'swank.swank)
     (@(ns-resolve '~'swank.swank '~'start-repl)
      (Integer. ~port)
      ~@(concat (map read-string opts)
                [:host host]))))

(defn swank
  "Launch swank server for Emacs to connect. Optionally takes PORT and HOST."
  ([project port host & opts]
     (eval-in-project project (swank-form project port host opts)))
  ([project port] (swank project port "localhost"))
  ([project] (swank project 4005)))
