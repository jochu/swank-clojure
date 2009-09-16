(ns swank.test-swank
  (:use clojure.test))

(def tests '(util
             util.net.sockets
             core.protocol
             commands.contrib.swank-c-p-c))

(def tests-ns
     (for [test tests]
       (symbol (str "swank.test-swank." test))))

(defn run-all []
  (println "Loading")
  (apply require :reload-all tests-ns)
  (apply run-tests tests-ns))