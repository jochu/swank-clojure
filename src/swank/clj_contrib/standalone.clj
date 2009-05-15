(ns swank.clj-contrib.standalone
  (:gen-class)
  (:use swank.swank)
  (:require clojure.main))

(defn- usage [msg]
  (.println System/err (str "ERROR: " msg "
usage: java -cp CLASSPATH swank.standalone [options]

where options are:

-p, --port PORT
        port on which to listen (required)

-f, --file PORT-FILE
        write listen port number to PORT-FILE; default=/dev/null

-v, --version PROTOCOL-VERSION
        slime protocol version

-e, --encoding ENCODING
        default=iso-latin-1-unix"))
  (System/exit 1))

(defn- -main [& args]
  (when (not (even? (count args)))
    (usage "Invalid number of arguments"))
  (let [argmap (apply hash-map args)
        announce-file (or (argmap "--file") (argmap "-f") "/dev/null")
        version (or (argmap "--version") (argmap "-v"))
        encoding (or (argmap "--encoding") (argmap "-e") "iso-latin-1-unix")
        port (or (argmap "--port") (argmap "-p"))]
    (when-not port (usage "Must specify port"))
    (when version (ignore-protocol-version version))
    (clojure.main/with-bindings
     (swank.swank/start-server announce-file :encoding encoding 
                               :port (Integer. #^String port)))))
