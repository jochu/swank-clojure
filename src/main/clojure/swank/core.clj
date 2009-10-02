(ns swank.core
  (:use (swank util commands)
        (swank.util hooks)
        (swank.util.concurrent thread)
        (swank.core connection hooks threadmap))
  (:require (swank.util.concurrent [mbox :as mb])))

;; Protocol version
(defonce *protocol-version* (ref nil))

;; Emacs packages
(def *current-package*)

(def #^{:doc "Include swank-clojure thread in stack trace for debugger."}
     *debug-swank-clojure* false)

(defonce *active-threads* (ref ()))

(defn maybe-ns [package]
  (cond
   (symbol? package) (or (find-ns package) (maybe-ns 'user))
   (string? package) (maybe-ns (symbol package))
   (keyword? package) (maybe-ns (name package))
   (instance? clojure.lang.Namespace package) package
   :else (maybe-ns 'user)))

(defmacro with-emacs-package [& body]
  `(binding [*ns* (maybe-ns *current-package*)]
     ~@body))

(defmacro with-package-tracking [& body]
  `(let [last-ns# *ns*]
     (try
      ~@body
      (finally
       (when-not (= last-ns# *ns*)
         (send-to-emacs `(:new-package ~(str (ns-name *ns*)) ~(str (ns-name *ns*)))))))))

(defmacro dothread-swank [& body]
  `(dothread-keeping-clj [*current-connection*]
     ~@body))

;; Exceptions for debugging
(defonce *debug-quit-exception* (Exception. "Debug quit"))
(def #^Throwable *current-exception*)

;; Handle Evaluation
(defn send-to-emacs
    "Sends a message (msg) to emacs."
    ([msg]
       (mb/send @(*current-connection* :control-thread) msg)))

(defn send-repl-results-to-emacs [val]
  (send-to-emacs `(:write-string ~(str (pr-str val) "\n") :repl-result)))

(defn eval-in-emacs-package [form]
  (with-emacs-package
   (eval form)))


(defn eval-from-control
  "Blocks for a mbox message from the control thread and executes it
   when received. The mbox message is expected to be a slime-fn."
  ([] (let [form (mb/receive (current-thread))]
        (apply (ns-resolve *ns* (first form)) (rest form)))))

(defn eval-loop
  "A loop which continuosly reads actions from the control thread and
   evaluates them (will block if no mbox message is available)."
  ([] (continuously (eval-from-control))))

(defn exception-causes [#^Throwable t]
  (lazy-seq
    (cons t (when-let [cause (.getCause t)]
              (exception-causes cause)))))

(defn- debug-quit-exception? [t]
  (some #(identical? *debug-quit-exception* %) (exception-causes t)))

(defn debug-loop
  "A loop that is intented to take over an eval thread when a debug is
   encountered (an continue to perform the same thing). It will
   continue until a *debug-quit* exception is encountered."
  ([] (try
       (eval-loop)
       (catch Throwable t
         ;; exit loop when not a debug quit
         (when-not (debug-quit-exception? t)
           (throw t))))))

(defn exception-stacktrace [#^Throwable t]
  (map #(list %1 %2 '(:restartable nil))
       (iterate inc 0)
       (map str (.getStackTrace t))))

(def *debug-thread-id*)
(defn invoke-debugger [#^Throwable thrown id]
  (dothread-swank
   (thread-set-name "Swank Debugger Thread")
   (binding [*current-exception* thrown
             *debug-thread-id* id]
     (let [level 1
           message (list (or (.getMessage thrown) "No message.")
                         (str "  [Thrown " (class thrown) "]")
                         nil)
           options `(("ABORT" "Return to SLIME's top level.")
                     ~@(when-let [cause (.getCause thrown)]
                         '(("CAUSE" "Throw cause of this exception"))))
           error-stack (exception-stacktrace thrown)
           continuations (list id)]
       (send-to-emacs (list :debug (current-thread) level message options error-stack continuations))
       (send-to-emacs (list :debug-activate (current-thread) level true))
       (debug-loop)
       (send-to-emacs (list :debug-return (current-thread) level nil))))))

(defn doall-seq [coll]
  (if (seq? coll)
    (doall coll)
    coll))

(defn eval-for-emacs [form buffer-package id]
  (try
   (binding [*current-package* buffer-package]
     (if-let [f (slime-fn (first form))]
       (let [form (cons f (rest form))
             result (doall-seq (eval-in-emacs-package form))]
         (run-hook *pre-reply-hook*)
         (send-to-emacs `(:return ~(thread-name (current-thread)) (:ok ~result) ~id)))
       ;; swank function not defined, abort
       (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))))
   (catch Throwable t
     ;; Thread/interrupted clears this thread's interrupted status; if
     ;; Thread.stop was called on us it may be set and will cause an
     ;; InterruptedException in one of the send-to-emacs calls below
     (Thread/interrupted)
     (set! *e t)

     ;; (.printStackTrace t #^java.io.PrintWriter *err*)
     ;; Throwing to top level, let emacs know we're aborting
     (when (debug-quit-exception? t)
       (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))
       (throw t))

     ;; start sldb, don't bother here because you can't actually recover with java
     (invoke-debugger (if *debug-swank-clojure*
                        t
                        (.getCause t))
                      id)
     ;; reply with abort
     (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id)))))

(defn- add-active-thread [thread]
  (dosync
   (commute *active-threads* conj thread)))

(defn- remove-active-thread [thread]
  (dosync
   (commute *active-threads* (fn [threads] (remove #(= % thread) threads)))))

(defn spawn-worker-thread
  "Spawn an thread that blocks for a single command from the control
   thread, executes it, then terminates."
  ([conn]
     (dothread-swank
       (try
        (add-active-thread (current-thread))
        (thread-set-name "Swank Worker Thread")
        (eval-from-control)
        (finally
         (remove-active-thread (current-thread)))))))

(defn spawn-repl-thread
  "Spawn an thread that sets itself as the current
   connection's :repl-thread and then enters an eval-loop"
  ([conn]
     (dothread-swank
      (thread-set-name "Swank REPL Thread")
      (with-connection conn
        (eval-loop)))))

(defn find-or-spawn-repl-thread
  "Returns the current connection's repl-thread or create a new one if
   the existing one does not exist."
  ([conn]
     ;; TODO - check if an existing repl-agent is still active & doesn't have errors
     (dosync
      (or (when-let [conn-repl-thread @(conn :repl-thread)]
            (when (.isAlive #^Thread conn-repl-thread)
              conn-repl-thread))
          (ref-set (conn :repl-thread)
                   (spawn-repl-thread conn))))))

(defn thread-for-evaluation
  "Given an id and connection, find or create the appropiate agent."
  ([id conn]
     (cond
      (= id true) (spawn-worker-thread conn)
      (= id :repl-thread) (find-or-spawn-repl-thread conn)
      :else (find-thread id))))

;; Handle control
(defn read-loop
  "A loop that reads from the socket (will block when no message
   available) and dispatches the message to the control thread."
  ([conn control]
     (with-connection conn
       (continuously (mb/send control (read-from-connection conn))))))

(defn dispatch-event
   "Dispatches/executes an event in the control thread's mailbox queue."
   ([ev conn]
      (let [[action & args] ev]
        (cond
          
         (= action :emacs-rex)
         (let [[form-string package thread id] args
               thread (thread-for-evaluation thread conn)]
           (mb/send thread `(eval-for-emacs ~form-string ~package ~id)))

         (= action :return)
         (let [[thread & ret] args]
           (binding [*print-level* nil, *print-length* nil]
             (write-to-connection conn `(:return ~@ret))))

         (one-of? action
                  :presentation-start :presentation-end
                  :new-package :new-features :ed :percent-apply :indentation-update
                  :eval-no-wait :background-message :inspect)
         (binding [*print-level* nil, *print-length* nil]
           (write-to-connection conn ev))

         (= action :write-string)
         (write-to-connection conn ev)

         (one-of? action
                  :debug :debug-condition :debug-activate :debug-return)
         (let [[thread & args] args]
           (write-to-connection conn `(~action ~(thread-map-id thread) ~@args)))

         (= action :emacs-interrupt)
         (let [[thread & args] args]
           (dosync
            (cond
              (and (true? thread) (seq @*active-threads*)) (.stop #^Thread (first @*active-threads*))
              (= thread :repl-thread) (.stop #^Thread @(conn :repl-thread)))))
         
         :else
         nil))))

;; Main loop definitions
(defn control-loop
  "A loop that reads from the mbox queue and runs dispatch-event on
   it (will block if no mbox control message is available). This is
   intended to only be run on the control thread."
  ([conn]
     (binding [*1 nil, *2 nil, *3 nil, *e nil]
       (with-connection conn
         (continuously (dispatch-event (mb/receive (current-thread)) conn))))))
