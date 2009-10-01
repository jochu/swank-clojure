;;;; swank-clojure.clj --- Swank server for Clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See README file for more information about installation
;;;

(ns swank.swank
  (:use (swank core)
        (swank.core connection server)
        (swank.util.concurrent thread))
  (:require (swank.util.concurrent [mbox :as mb])
            (swank commands)
            (swank.commands basic indent completion
                            contrib inspector import)
            (swank.commands.contrib trace)))

(defn ignore-protocol-version [version]
  (dosync (ref-set *protocol-version* version)))


(defn- connection-serve [conn]
  (let [control
        (dothread-swank
          (thread-set-name "Swank Control Thread")
          (try
           (control-loop conn)
           (catch Exception e
             ;; fail silently
             nil)))
        read
        (dothread-swank
          (thread-set-name "Read Loop Thread")
          (try
           (read-loop conn control)
           (catch Exception e
             ;; This could be put somewhere better
             (.interrupt control)
             (dosync (alter *connections* (partial remove #{conn}))))))]
    (dosync
     (ref-set (conn :control-thread) control)
     (ref-set (conn :read-thread) read))))

(defn start-server
  "Start the server and write the listen port number to
   PORT-FILE. This is the entry point for Emacs."
  ([port-file & opts]
     (let [opts (apply hash-map opts)]
       (setup-server (get opts :port 0)
                     (fn announce-port [port]
                       (announce-port-to-file port-file port)
                       (simple-announce port))
                     connection-serve
                     opts))))
