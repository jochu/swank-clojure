(ns leiningen.swank
  (:use [leiningen.compile :only [eval-in-project]]))

(defn swank-form [port host opts]
  `(do (require '~'swank.swank)
       (@(ns-resolve '~'swank.swank '~'start-repl)
        (Integer. ~port)
        ~@(concat (map read-string opts)
                  [:host host]))))

(defn swank
  "Launch swank server for Emacs to connect. Optionally takes PORT and HOST."
  ([project port host & opts]
     (eval-in-project project (swank-form post host opts)))
  ([project port] (swank project port "localhost"))
  ([project] (swank project 4005)))
