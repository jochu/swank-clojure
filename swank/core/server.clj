(ns swank.core.server
  (:use (swank util core)
        (swank.util sys io)
        (swank.util.concurrent thread)
        (swank.util.net sockets)
        (swank.core connection protocol))
  (:import (java.io File FileReader BufferedReader InputStreamReader OutputStreamWriter)
           (java.net Socket)))

;; The swank.core.server is the layer above swank.util.net.sockets
;;  - Manages the socket server
;;  - Accepts and authenticates incoming connections
;;  - Creates swank.core.connections
;;  - Spins up new threads

(defonce *connections* (ref []))

(defn- slime-secret
  "Load the data from the secret file. Returns nil if secret file
   could not be read."
  ([] (try
       (let [path-to-secret (str (user-home-path) File/separator ".slime-secret")]
         (with-open [secret (BufferedReader. (FileReader. path-to-secret))]
           (.readLine secret)))
       (catch Throwable e nil))))

(defn- accept-authenticated-connection
  "If a slime-secret file exists, verify that the incomming connection
   has sent it and the authentication string matches."
  ([#^Socket socket opts]
     (returning conn (make-connection socket (get opts :encoding *default-encoding*))
       (when-let [secret (slime-secret)]
         (let [first-val (read-from-connection conn)]
           (when-not (= first-val secret)
             (.close socket)
             (throw (new Exception "Incoming connection doesn't know the password."))))))))

(defn- make-output-redirection
  ([conn]
     (call-on-flush-stream
      #(with-connection conn
         (send-to-emacs `(:write-string ~%)))))
  {:tag java.io.StringWriter})

(def dont-close nil)

(defn- socket-serve [connection-serve socket opts]
  (with-connection (accept-authenticated-connection socket opts)
    (let [out-redir (make-output-redirection *current-connection*)]
      (binding [*out* out-redir
                *err* (java.io.PrintWriter. out-redir)]
        (dosync (ref-set (*current-connection* :writer-redir) *out*))
        (dosync (alter *connections* conj *current-connection*))
        (connection-serve *current-connection*)
        (not dont-close)))))


;; Setup frontent
(defn setup-server
  "Starts a server. The port it started on will be called as an
  argument to (announce-fn port). A connection will then be created
  and (connection-serve conn) will then be called."
  ([port announce-fn connection-serve opts]
     (let [ss (socket-server port #(socket-serve connection-serve % opts))
           local-port (.getLocalPort ss)]
       (announce-fn local-port)
       local-port)))

;; Announcement functions
(defn simple-announce [port]
  (println "Connection opened on local port " port))

(defn announce-port-to-file
  "Writes the given port number into a file."
  ([#^String file port]
     (with-open [out (new java.io.FileWriter file)]
       (doto out
         (.write (str port "\n"))
         (.flush)))))
