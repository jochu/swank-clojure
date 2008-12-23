(ns swank.util.net.sockets
  (:use (swank util)
        (swank.util.concurrent thread))
  (:import (java.net ServerSocket Socket SocketException InetAddress)))

(defn socket-server
  ([port handle-socket]
     (returning server (ServerSocket. port)
       (dothread-keeping-clj nil
         (thread-set-name (str "Server " port " [" (thread-id) "]"))
         (with-open [server server]
           (loop []
             (when-not (.isClosed server)
               (try
                (when (handle-socket (.accept server))
                  (recur))
                (catch SocketException e 
                  (.close server)))))))))
  {:tag ServerSocket})

(defn close-socket
  "Cleanly shutdown and close the socket."
  ([#^Socket s]
     (.shutdownInput s)
     (.shutdownOutput s)
     (.close s)))
