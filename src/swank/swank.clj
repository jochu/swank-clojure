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
  (:use [swank.core]
        [swank.core connection server]
        [swank.util.concurrent thread]
        [clojure.main :only [repl]])
  (:require [swank.commands]
            [swank.commands basic indent completion
             contrib inspector])
  (:import [java.lang System]
           [java.io File])
  (:gen-class))

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

(defn start-repl
  "Start the server wrapped in a repl. Use this to embed swank in your code."
  ([port & opts]
     (let [stop (atom false)
           opts (merge {:port port
                        :encoding (or (System/getProperty
                                       "swank.encoding")
                                      "iso-latin-1-unix")}
                       (apply hash-map opts))]
       (repl :read (fn [rprompt rexit]
                     (if @stop rexit
                         (do (swap! stop (fn [_] true))
                             `(do (ignore-protocol-version nil)
                                  (start-server (-> "java.io.tmpdir"
                                                    (System/getProperty)
                                                    (File. "slime-port.txt")
                                                    (.getCanonicalPath))
                                                ~@(apply concat opts))))))
             :need-prompt #(identity false))))
  ([] (start-repl 4005)))

(def -main start-repl)
