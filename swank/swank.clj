;;;; swank-clojure.clj --- Swank server for Clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See README file for more information about installation
;;;

(clojure/ns swank
  (:use (swank core)
        (swank.core connection server)
        (swank.util.concurrent thread))
  (:require (swank.util.concurrent [mbox :as mb])
            (swank commands)
            (swank.commands basic indent contrib inspector)))

(defn ignore-protocol-version [version]
  (dosync (ref-set *protocol-version* version)))


(defn- connection-serve [conn]
  (let [control
        (dothread-keeping [*out* *ns* *current-connection* *warn-on-reflection*]
          (thread-set-name "Swank Control Thread")
          (control-loop conn))
        read
        (dothread-keeping [*out* *ns* *current-connection* *warn-on-reflection*]
          (thread-set-name "Read Loop Thread")
          (read-loop conn control))]
    (dosync
     (ref-set (conn :control-thread) control)
     (ref-set (conn :read-thread) read))))

(defn start-server
  "Start the server and write the listen port number to
   PORT-FILE. This is the entry point for Emacs."
  ([port-file & opts]
     (binding [*warn-on-reflection* true]
       (let [opts (apply hash-map opts)]
         (setup-server (get opts :port 0)
                       (fn announce-port [port]
                         (announce-port-to-file port-file port)
                         (simple-announce port))
                       connection-serve)))))

(comment
;;;;;;;;;;;;;;;;; commands ;;;;;;

  (defn swank-require [& shrug] nil)

;;;; Macro expansion

;;;; Packages

;;;; Indentation

  ;; Source Locations

;;;;;;;;;;;;;;;;;

  (defn- debug-loop
    "An agent loop which constantly evaluates messages from the control
   thread. When a debug exception is thrown, abort and quit."
    ([th]
       (try
        (eval-from-control)
        (send-off-with-bindings *agent* #'eval-loop)
        (catch Throwable t
          ;; Stop looping when an exception is thrown and tell 'em the debug is over
          (comment (send-to-emacs (list :debug-return (current-thread) level nil)))))
       th))

  (defn debug-thread [t]
    (try
     (loop []
       (eval-from-control)
       (recur))
     (catch Throwable t nil))
    )


  (defn- eval-for-emacs
    "Handle an :emacs-rex command by executing the form. The results of
   the execution are returned as :ok or :abort. See dispatch-event."
    ([form package id]
       (try
        (binding [*package* package]
          (let [result (eval form)]
            (run-hook *pre-reply-hook*)
            (send-to-emacs `(:return ~(str (hash *agent*))
                                     (:ok ~result)
                                     ~id))))
        (catch Throwable t
          (send-to-emacs `(:write-string ~(.toString t)))
          (send-to-emacs `(:return ~(str (hash *agent*))
                                   (:abort)
~id))))))

;;;;;;;;;;;;;;;;;

(defn buffer-first-change
  "Source file change, undeeded."
  ([& ignore] nil))
)
