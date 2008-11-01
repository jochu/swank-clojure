(clojure/ns swank.util.net.sockets
  (:use (swank util)
        (swank.util.concurrent thread))
  (:import (java.net ServerSocket Socket InetAddress)))

(defn socket-server [port handle-socket]
  (returning server (ServerSocket. port)
    (dothread-keeping [*out* *ns* *1 *2 *3 *e
                       *warn-on-reflection* *print-level* *print-length*]
      (thread-set-name (str "Server " port " [" (thread-id) "]"))
      (with-open server server
        (loop []
          (when-not (.isClosed server)
            (when (handle-socket (.accept server))
              (recur))))))))
