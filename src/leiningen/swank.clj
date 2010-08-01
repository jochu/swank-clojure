(ns leiningen.swank
  (:use [leiningen.compile :only [eval-in-project]]
        [leiningen.core :only [get-global-init-script]])
  (:import [java.io File]))

(defn swank
  "Launch swank server for Emacs to connect. Optionally takes PORT and HOST."
  ([project port host & opts]
     (eval-in-project project
                      `(do
                         (let [is# ~(:init-script project)
                               gis# ~(get-global-init-script)]
                           (when (not (nil? gis#))
                             (load-file gis#))
                           (when (and (not (nil? is#)) (.exists (File. (str is#))))
                             (load-file is#)))
                         (require '~'swank.swank)
                         (@(ns-resolve '~'swank.swank '~'start-repl)
                          (Integer. ~port)
                          ~@(concat (map read-string opts)
                                    [:host host])))))
  ([project port] (swank project port "localhost"))
  ([project] (swank project 4005)))
