(clojure/ns swank.core.connection
  (:use (swank util)
        (swank.util sys)
        (swank.core protocol))
  (:import (java.net ServerSocket Socket InetAddress)
           (java.io InputStreamReader OutputStreamWriter)))

(def *current-connection*)

(defmacro with-connection [conn & body]
  `(binding [*current-connection* ~conn] ~@body))

(defn make-connection [#^Socket socket]
  {:socket socket
   :reader (InputStreamReader. (.getInputStream socket))
   :writer (OutputStreamWriter. (.getOutputStream socket))
   :writer-redir (ref nil)
   :indent-cache (ref {})
   :indent-cache-pkg (ref nil)
   :control-thread (ref nil)
   :read-thread (ref nil)
   :repl-thread (ref nil)})

(defn read-from-connection
  ([] (read-from-connection *current-connection*))
  ([conn]
     (read-swank-message (conn :reader)))
  {:tag String})

(defn write-to-connection
  ([msg] (write-to-connection *current-connection* msg))
  ([conn msg]
     (write-swank-message (conn :writer) msg)))
