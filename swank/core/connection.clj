(ns swank.core.connection
  (:use (swank util)
        (swank.util sys)
        (swank.core protocol))
  (:import (java.net ServerSocket Socket InetAddress)
           (java.io InputStreamReader OutputStreamWriter)))

(def *current-connection*)
(def *default-encoding* "iso-8859-1")

(defmacro with-connection [conn & body]
  `(binding [*current-connection* ~conn] ~@body))

(def encoding-map
     {"latin-1" "iso-8859-1"
      "latin-1-unix" "iso-8859-1"
      "iso-latin-1-unix" "iso-8859-1"
      "iso-8859-1" "iso-8859-1"
      "iso-8859-1-unix" "iso-8859-1"

      "utf-8" "utf-8"
      "utf-8-unix" "utf-8"

      "euc-jp" "euc-jp"
      "euc-jp-unix" "euc-jp"

      "us-ascii" "us-ascii" 
      "us-ascii-unix" "us-ascii"})

(defn make-connection
  ([#^Socket socket] (make-connection socket *default-encoding*))
  ([#^Socket socket encoding]
     {:socket socket
      :reader (InputStreamReader. (.getInputStream socket) #^String (get encoding-map encoding encoding))
      :writer (OutputStreamWriter. (.getOutputStream socket) #^String (get encoding-map encoding encoding))
      :writer-redir (ref nil)
   
      :indent-cache (ref {})
      :indent-cache-pkg (ref nil)
   
      :control-thread (ref nil)
      :read-thread (ref nil)
      :repl-thread (ref nil)}))

(defn read-from-connection
  ([] (read-from-connection *current-connection*))
  ([conn]
     (read-swank-message (conn :reader)))
  {:tag String})

(defn write-to-connection
  ([msg] (write-to-connection *current-connection* msg))
  ([conn msg]
     (write-swank-message (conn :writer) msg)))
