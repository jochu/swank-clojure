;;;; swank-clojure.clj --- Swank server for Clojure
;; Load up genclass 
;; (clojure/in-ns 'clojure)
;; (binding [*warn-on-reflection* nil] ;; suppress the warnings!
;;   (. clojure.lang.RT loadResourceScript "genclass.clj"))

(in-ns 'swank)

;;;; General coding utils

; (put 'flet 'clojure-indent-function 1)
(defmacro flet
  "Allows for local function definitions in the following
   format: (flet [(fn name [args] .. body)] .. )."
  ([fns & body]
     (let [fn-name
           (fn fn-name [fn-seq]
             (second fn-seq))
           defs (apply vector (mapcat list (map fn-name fns) fns))]
       `(let ~defs ~@body))))

(defmacro one-of?
  "Short circuiting value comparison."
  ([val & possible]
     (let [v (gensym)]
       `(let [~v ~val]
          (or ~@(map (fn [p] `(= ~v ~p)) possible))))))

(defn ffilter
  "Returns the first entry that's true for fn"
  ([coll] (ffilter identity coll))
  ([fn coll]
     (first (filter fn coll))))

(defn position
  "Finds the first position of item within col. Returns nil if not
   found."
  ([item coll]
     (loop [coll coll, i 0]
       (when coll
         (if (= (first coll) item)
           i
           (recur (rest coll) (inc i))))))
  {:tag Integer})

(defn mape
  "Maps a map, the given function will recieve a map-entity."
  ([f hsh]
     (mape f {} hsh))
  ([f init hsh]
     (with-meta
      (reduce #(assoc %1 (key %2) (f %2)) init hsh)
      (meta hsh))))

(defn mapv
  "Maps a map, the given function will recieve each value"
  ([f hsh]
     (mape (comp f val) hsh))
  ([f init hsh]
     (mape (comp f val) init hsh)))

(defn categorize-by
  "Categorizes elements within a coll into a map based on a keyfn."
  ([keyfn coll]
     (categorize-by keyfn {} coll))
  ([keyfn init coll]
     (reduce #(let [key (keyfn %2)
                    val (conj (get %1 key []) %2)]
                (assoc %1 key val))
             init coll)))

(defmacro if-let*
  ([bindings then]
     `(if-let* ~bindings ~then nil))
  ([bindings then else]
     (if bindings
       `(if-let ~@(take 2 bindings)
            (if-let* ~(drop 2 bindings) ~then ~else)
          ~else)
       then))
  {:indent 1})




;;;; Global variables / constants / configs

;; The default server port
(def #^Integer *default-server-port* 4005)

;; The default charset of the connections (not used)
; (def #^String *default-charset* "iso-8859-1")

;; Whether debug is on
(def #^Boolean *swank-debug?* false)

;; Whether to use redirect io or not
(def #^Boolean *redirect-io* true)

;; Whether to use dedicated output stream or not
(def *use-dedicated-output-stream* nil)

;; What port to use if dedicated output stream is enabled
(def *dedicated-output-stream-port* nil)

;; The default communication style
(def *communication-style* :spawn)

;; Whether to stop accepting connections after the first or not
(def *dont-close* nil)

;; The default coding system
(def *coding-system* "iso-8859-1")

;; A map of all listener sockets, with ports as the key
(defonce *listener-sockets* (ref {}))

;; Whether to log or not
(def *log-events* false)

;; Where to log the output
(def #^PrintStream *log-output* (. System err))

;; The loopback interace address
(def *loopback-interface* "127.0.0.1")

;; A list of all connections for this swank server
(defonce *connections* (ref nil))

;; The current emacs connection
(def *emacs-connection* nil)

;; The regular expression that matches namespace so it may be rewritten
(def *namespace-re* (re-pattern "(^\\(:emacs-rex \\([a-zA-Z][a-zA-Z0-9]+):"))

;; The running unique ids of input requests
(defonce *read-input-catch-tag* (ref 0))

;; A map of catch-tag ids and exceptions
(defonce *read-input-catch-tag-intern* (ref {}))

;; The state stack, whatever that is
(def *swank-state-stack* nil)

;; The package in which all swank io is read with
(def *swank-io-package* (create-ns 'swank-io-package))

;; A list of currently active threads
(def *active-threads* (ref nil))

;; The protocol version (to ignore)
(def *protocol-version* (ref "just say yes"))

;; Whether or not to set emacs indentation
(def *configure-emacs-indentation* true)


;;;; Threads

(defn spawn
  ([#^Runnable f]
     (spawn f (str (gensym "Swank-Thread"))))
  ([#^Runnable f #^String name]
     (let [calling-ns *ns*
           calling-out *out*]
       (doto (new Thread
                  (fn []
                    (binding [*ns* calling-ns
                              *out* calling-out]
                      (f)))
                  name)
         (start)))))

(defn current-thread
  ([]
     (. Thread currentThread))
  {:tag Thread})

(defn thread-name [#^Thread t]
  (. t getName))

(defn sleep-milli
  "Sleeps for a given number of milliseconds"
  ([#^Long time]
     (. Thread sleep time)))

(defn sleep
  "Sleeps for a given number of seconds"
  ([#^Integer time]
     (sleep-milli (* time 1000))))

(defn list-threads
  "Returns a list of currently running threads. It's a pretty sketchy
   way of doing it right now, but it seems to work."
  ([]
     (map key (seq (. Thread getAllStackTraces)))))

(defn find-thread-by-name
  ([name]
     (ffilter #(= name (thread-name %)) (list-threads)))
  {:tag Thread})

(defn thread-alive?
  ([#^Thread thread]
     (. thread isAlive))
  {:tag Boolean})

(defn kill-thread
  ([#^Thread thread]
     (. thread interrupt)))

(def *thread-map-next-id* (ref 1))
(def *thread-map* (ref {}))

(defn thread-map-clean []
  (doseq [id t] @*thread-map*
    (when (or (nil? t)
              (not (thread-alive? t)))
      (dosync
       (alter *thread-map* dissoc id)))))

(defn thread-id [thread]
  (let [id (dosync 
            (if-let entry (ffilter #(= (val %) thread) @*thread-map*)
              (key entry)
              (let [next-id @*thread-map-next-id*]
                (alter *thread-map* assoc next-id thread)
                (alter *thread-map-next-id* inc)
                next-id)))]
    (thread-map-clean)
    id))

(defn find-thread [id]
  (@*thread-map* id))

(defn uncaught-exception-handler
  ([f]
     (proxy [java.lang.Thread$UncaughtExceptionHandler] []
       (uncaughtException [th ex]
         (f th ex))))
  {:tag java.lang.Thread$UncaughtExceptionHandler})

;; May be redefined by swank-clojure-debug
(defn use-uncaught-exception-handler [f]
  (. (current-thread)
     setUncaughtExceptionHandler
     (uncaught-exception-handler f)))


;;;; Socket programming

(defn create-socket
  "Creates a ServerSocket based on a host and port"
  ([host port]
     (new ServerSocket port 0 (. InetAddress getByName host)))
  {:tag ServerSocket})

(defn local-port
  "Returns the local port of a ServerSocket."
  ([#^ServerSocket socket]
     (. socket getLocalPort))
  {:tag Integer})

(defn close-socket [#^ServerSocket socket]
  (. socket close))

(defn accept-connection
  "Accepts an incoming connection on a ServerSocket"
  ([#^ServerSocket socket]
     (. socket accept))
  {:tag Socket})


;;;; IO / Stream utils

(defn read-chars
  "Reads a given amount of characters out of a reader."
  ([#^java.io.InputStream reader #^Integer num-chars]
     (let [sb (new StringBuilder num-chars)]
       (dotimes count num-chars
         (. sb (append (char (. reader (read))))))
       (str sb)))
  {:tag String})

(defn read-from-string
  "Reads the next object from a string."
  ([string]
     (with-open rdr (new LineNumberingPushbackReader (new StringReader string))
       (read rdr))))


;;;; Logging
(defn log-event [& strs]
  (when *log-events*
    (doto *log-output*
      (print (apply str strs))
      (flush))))


;;;; System interface

(defn get-pid
  "Returns the PID of the JVM. This may or may not be accurate
   depending on the JVM in which clojure is running off of."
  ([]
     (or (first (.. java.lang.management.ManagementFactory (getRuntimeMXBean) (getName) (split "@")))
         (. System (getProperty "pid"))))
  {:tag String})

(defn user-home-path []
  (. System getProperty "user.home"))


;;;; Swank utils

(defn fix-namespace
  ([text]
     (let [m (re-matcher *namespace-re* text)]
       (. m (replaceAll "$1/")))))

(defn hex->num
  "Converts a hex string into an integer"
  ([#^String hex-str]
     (. Integer (parseInt hex-str 16)))
  {:tag Integer})

(defn num->hex
  "Converts a number to a hex string. If a minimum length is provided,
   the hex number will be left padded with 0s."
  ([num]
     (. Integer (toHexString num)))
  ([num min-len]
     (let [hex (num->hex num)
           len (count hex)]
       (if (< len min-len)
         (str (apply str (replicate (- min-len len) \0)) hex)
         hex)))
  {:tag String})

(defn ignore-protocol-version [version]
  (dosync (ref-set *protocol-version* version)))


;;;; Hooks

(defn add-hook [place function]
  (dosync
   (alter place conj function)))

(defn run-hook [functions & arguments]
  (doseq f @functions
    (apply f arguments)))

;; Pre reply hooks
(def *pre-reply-hook* (ref nil))


;;;; Bad stream mojo (but better than before)
(defn out-fn-stream [out-fn]
  (let [closed? (ref nil)
        #^StringWriter stream
        (proxy [java.io.StringWriter] []
          (close []
            (dosync
             (ref-set closed? true)))
          (flush []
            (let [#^StringWriter me this] ;; only so it know what it is
              (let [len (.. me getBuffer length)]
                (when (> len 0)
                  (out-fn (.. me getBuffer (substring 0 len)))
                  (.. me getBuffer (delete 0 len)))))))]
   (spawn
    (fn []
      (loop []
        (. Thread (sleep 200))
        (when-not @closed?
          (. stream flush)
          (recur))))
    (str (gensym "Swank out stream flusher ")))
   stream))



;;;; Raw Message Encoding
(defn maybe-out-stream
  ([stream]
     (cond
      (instance? Socket stream) (let [#^Socket s stream] (. s getOutputStream))
      :else stream))
  {:tag OutputStream})

(defn encode-message
  "Encodes a message into SWANK format"
  ([message stream]
     (log-event "*jochu* ENCODE MESSAGE " (pr-str message) "\n")
     (log-event "*jochu*      TO STREAM " (pr-str stream) "\n")
     (let [#^String string (pr-str message)
           length (count string)]
       (doto (maybe-out-stream stream)
         (write (. (num->hex length 6) getBytes))
         (write (. string getBytes *coding-system*))
         (flush)))))

(defn read-form [string]
  (binding [*ns* *swank-io-package*]
    (read-from-string string)))

(defn decode-message-length [#^Socket stream]
  (let [string (read-chars (. stream getInputStream) 6)]
    (hex->num string)))


(defn decode-message
  "Read an s-expression from stream using the slime protocol"
  ([#^Socket stream]
     (binding [*swank-state-stack* (cons :read-next-form *swank-state-stack*)]
       (let [length (decode-message-length stream)
             string (read-chars (. stream getInputStream) length)]
         (log-event "*jochu* READ: " string "\n")
         (read-form (fix-namespace string))))))


;;;; Mailboxes (message queues)

;; Holds references to the mailboxes (message queues) for a thread
(def *mailboxes* (ref {}))

(defn mbox
  "Returns a mailbox for a thread. Creates one if one does not already exist."
  ([#^Thread thrd]
     (dosync
      (when-not (@*mailboxes* thrd)
        (alter
         *mailboxes* assoc
         thrd (new java.util.concurrent.LinkedBlockingQueue))))
     (@*mailboxes* thrd))
  {:tag java.util.concurrent.LinkedBlockingQueue})

(defn mb-send
  "Sends a message to a given thread"
  ([#^Thread thrd message]
     (let [mbox (mbox thrd)]
       (. mbox put message))))

(defn mb-receive
  "Blocking recieve for messages for the current thread"
  ([]
     (let [mb (mbox (current-thread))]
       (. mb take))))



;;;; Swank connection object

(defstruct connection
  ;; Raw I/O stream of socket connection.
  :socket-io
  ;; Optional dedicated output socket (backending `user-output' slot).
  ;; Has a slot so that it can be closed with the connection.
  :dedicated-output nil
  ;; Streams that can be used for user interaction, with requests
  ;; redirected to Emacs.
  :user-input :user-output :user-io
  ;; A stream that we use for *trace-output*; if nil, we user user-output.
  :trace-output
  ;; A stream where we send REPL results.
  :repl-results
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them; the `repl-thread' is the one
  ;; that evaluates REPL expressions. The control thread dispatches
  ;; all REPL evaluations to the REPL thread and for other requests it
  ;; spawns new threads.
  :reader-thread :control-thread :repl-thread
  ;; Callback functions:
  ;; (SERVE-REQUESTS <this-connection>) serves all pending requests
  ;; from Emacs.
  :server-requests
  ;; (READ) is called to read and return one message from Emacs.
  :read
  ;; (SEND OBJECT) is called to send one message to Emacs.
  :send
  ;; (CLEANUP <this-connection>) is called when the connection is
  ;; closed.
  :cleanup
  ;; Cache of macro-indentation information that has been sent to Emacs.
  ;; This is used for preparing deltas to update Emacs's knowledge.
  ;; Maps: symbol -> indentation-specification
  :indentation-cache
  ;; The list of packages represented in the cache:
  :indentation-cache-packages
  ;; The communication style used.
  :communication-style
  ;; The coding system for network streams.
  :coding-system)


;;;; Communication between emacs and swank server

;;; Send to emacs
(defn send-to-emacs
  "Send an object to the current emacs connection"
  ([obj]
     ((*emacs-connection* :send) obj)))

(defn send-oob-to-emacs [obj]
  (send-to-emacs obj))

(def send-oob-to-emacs #'send-to-emacs)

(defn send-to-control-thread
  "Send an object to the current emacs-connection's control-thread"
  ([obj]
     (mb-send @(*emacs-connection* :control-thread) obj)))

;;; Read from emacs
(defn read-from-control-thread []
  (mb-receive))

(defn read-from-emacs []
  (let [call ((*emacs-connection* :read))]
    (log-event "*jochu* CALL:" (pr-str (doall call)) "\n")
    (apply (find-var (first call)) (rest call))))

(defn intern-catch-tag [tag]
  (dosync
   (when-not (@*read-input-catch-tag-intern* tag)
     (alter *read-input-catch-tag-intern*
            assoc tag (new Exception))))
  (@*read-input-catch-tag-intern* tag))

(defn read-user-input-from-emacs []
  (let [tag (dosync (alter *read-input-catch-tag* inc))]
    (send-to-emacs `(:read-string ~(thread-name (current-thread)) ~tag))
    (try
     (loop [] (read-from-emacs) (recur))
     (catch Exception e
       (let [#^Exception e e]
         (if (= (. e getCause) (intern-catch-tag tag))
           (. e getMessage)
           (throw e))))
     (catch Throwable t
       (send-to-emacs `(:read-aborted ,(thread-name (current-thread)) ~tag))))))

(defn y-or-n-p-in-emacs [& strs]
  (let [tag (dosync (alter *read-input-catch-tag* inc))
        question (apply str strs)]
    (send-to-emacs `(:y-or-n-p ~(thread-name (current-thread)) ~tag ~question))
    (try
     (loop [] (read-from-emacs) (recur))
     (catch Exception e
       (let [#^Exception e e]
         (if (= (. e getCause) (intern-catch-tag tag))
           (. e getMessage)
           (throw e))))
     (catch Throwable t
       (send-to-emacs `(:read-aborted ,(thread-name (current-thread)) ~tag))))))

(defn take-input
  "Return the string input to the continuation tag"
  ([tag #^String input]
     (throw (new Exception input (intern-catch-tag tag)))))


;;;; IO Redirection support

(defmacro with-io-redirection
  "Executes the body and redirects all output into an output stream if
   redirect-io is enabled."
  ([connection & body]
     `(maybe-call-with-io-redirection ~connection (fn [] ~@body))))

(defn call-with-redirected-io [connection f]
  (binding [*out* (connection :user-output)]
    (f)))

(defn maybe-call-with-io-redirection [connection f]
  (if *redirect-io*
    (call-with-redirected-io connection f)
    (f)))


;;;; Making connections

(defn default-connection []
  (first @*connections*))

;; prototype (defined later)
(def accept-authenticated-connection)

(defmacro with-connection [connection & body]
  `(binding [*emacs-connection* ~connection]
     (with-io-redirection *emacs-connection*
       ~@body)))

(defn open-dedicated-output-stream
  ([socket-io]
     (let [socket (create-socket *loopback-interface* *dedicated-output-stream-port*)
           port (local-port socket)]
       (try
        (encode-message '(:open-dedicated-output-stream ~port) socket-io)
        (let [dedicated (accept-authenticated-connection socket)]
          (close-socket socket)
          dedicated)
        (finally
         (when (and socket (not (. socket isClosed)))
           (close-socket socket))))))
  {:tag OutputStream})

(defn make-output-function
  "Create function to send user output to Emacs. This function may
   open a dedicated socket to send output. 

   It returns [the output function, the dedicated stream (or nil if
   none was created)]"
  ([connection]
     (if *use-dedicated-output-stream*
       (let [stream (open-dedicated-output-stream (connection :socket-io))]
         ;; todo - This doesn't work...
         [(fn [#^String string] (doto stream (write (. string getBytes)) (flush))) stream])
       [(fn [string] (with-connection connection (send-to-emacs `(:write-string ~string)))) nil])))


(defn open-streams
  "Return the 5 streams for IO redirection: 
     :dedicated-output, :input, :output, :io, :repl-results"
  ([connection]
     (let [[output-fn dedicated-output] (make-output-function connection)
           input-fn (fn [] (with-connection connection (read-user-input-from-emacs)))
           repl-results (fn [string] (with-connection connection (send-to-emacs `(:write-string ~string :repl-result))))]
       [dedicated-output input-fn output-fn [input-fn output-fn] repl-results])))

(defn initialize-streams-for-connection
  "Initiliazes streams needed for a connection (dedicated-output,
   input, output, io, and repl-results)"
  ([connection]
     (let [[dedicated in out io repl] (open-streams connection)]
       (assoc connection
         :dedicated-output dedicated
         :user-input in
         :user-output (out-fn-stream out)
         :user-io io
         :repl-results repl))))

(defn handle-request
  "Read and process on request. The processing is done in the extent
  of the toplevel restart."
  ([connection]
     (binding [*swank-state-stack* '(:handle-request)]
       (read-from-emacs))))

(defn repl-loop [connection]
  (loop [] (handle-request connection) (recur)))

(defn spawn-worker-thread [connection]
  ;; something about with-bindings business
  (spawn (fn [] (with-connection connection (handle-request connection)))
         (str (gensym "Swank worker thread "))))

(defn spawn-repl-thread [connection name]
  (spawn (fn [] (with-connection connection (repl-loop connection)))
         name))

(defn repl-thread [connection]
  (let [thread @(connection :repl-thread)]
    (when-not thread
      (log-event "ERROR: repl-thread is nil"))
    (cond
     (thread-alive? thread) thread
     :else (dosync (ref-set (connection :repl-thread)
                            (spawn-repl-thread connection "Swank REPL Thread"))))))

(defn thread-for-evaluation [id]
  (cond
   (= id 't) (spawn-worker-thread *emacs-connection*)
   (= id :repl-thread) (repl-thread *emacs-connection*)
   :else (find-thread id)))

(defn find-worker-thread [id]
  (cond
   (one-of? id 't true) (first @*active-threads*)
   (= id :repl-thread) (repl-thread *emacs-connection*)
   :else (find-thread id)))

(defn interrupt-worker-thread [id]
  (let [#^Thread thread (or (find-worker-thread id)
                            (repl-thread *emacs-connection*))]
    (if (. thread isInterrupted)
      (. thread stop) ;; This is unsafe -- even deprecated; but if you really want it dead...
      (do
        (send-to-emacs '(:background-message "Abort again if thread does not stop. May be unsafe if IO locks exist."))
        (. thread interrupt)))))

(defn dispatch-event [event socket-io]
  (log-event "DISPATCHING: " (pr-str event) "\n")
  (let [[ev & args] event]
    (cond
     (= ev :emacs-rex)
     (let [[form package thread-id id] args]
       (let [thread (thread-for-evaluation thread-id)]
         (dosync ;; add thread to active-threads
          (alter *active-threads* conj thread))
         (mb-send thread `(eval-for-emacs ~form ~package ~id))))
     
     (= ev :return)
     (let [[thread & args] args]
       (dosync ;; Remove thread from active-threads
        (let [n (count @*active-threads*)
              remaining (filter #(not= (thread-name %) thread) @*active-threads*)]
          (ref-set *active-threads* remaining)))
       (encode-message `(:return ~@args) socket-io))

     (= ev :emacs-interrupt)
     (let [[thread-id] args]
       (interrupt-worker-thread thread-id))

     (one-of? ev
              :debug :debug-condition :debug-activate :debug-return)
     (let [[thread & args] args]
       (encode-message `(~ev ~(thread-id thread) ~@args) socket-io))
     
     (one-of? ev
              :write-string :presentation-start :presentation-end
              :new-package :new-features :ed :percent-apply :indentation-update
              :eval-no-wait :background-message :inspect)
     (encode-message event socket-io)
     
     :else (log-event "*jochu* UNHANDLED EVENT " (pr-str event) "\n"))))

(defn dispatch-loop [socket-io connection]
  (binding [*emacs-connection* connection]
    (loop []
      (dispatch-event (mb-receive) socket-io)
      (recur))))

(defn read-loop [control-thread input-stream connection]
  ;; with-reader-error-handler
  (loop [] (mb-send control-thread (decode-message input-stream)) (recur)))

(defn spawn-threads-for-connection [connection]
  (let [socket-io (connection :socket-io)
        control-thread (spawn (fn [] (dispatch-loop socket-io connection)) "Swank Control Thread")]
    (dosync
     (ref-set (connection :control-thread) control-thread))
    (let [reader-thread (spawn (fn []
                                 (let [go (mb-receive)]
                                   (assert (= go 'accept-input)))
                                 (read-loop control-thread socket-io connection))
                               "Swank Reader Thread")
          repl-thread (spawn-repl-thread connection "Swank REPL Thread")]
      (dosync
       (ref-set (connection :repl-thread) repl-thread)
       (ref-set (connection :reader-thread) reader-thread))
      (mb-send reader-thread 'accept-input)
      connection)))

(defn cleanup-connection-threads [connection]
  (let [threads (map connection [:repl-thread :reader-thread :control-thread])]
    (doseq thread threads
      (when (and thread
                 (thread-alive? thread)
                 (not= (current-thread) thread))
        (kill-thread thread)))))

(defn create-connection [socket-io style]
  (initialize-streams-for-connection
   (cond
    (= style :spawn) (struct-map connection
                       :socket-io socket-io
                       :read read-from-control-thread
                       :send send-to-control-thread
                       :serve-requests spawn-threads-for-connection
                       :cleanup cleanup-connection-threads
                       :communication-style style
                       :control-thread (ref nil)
                       :reader-thread (ref nil)
                       :repl-thread (ref nil)
                       :indentation-cache (ref {})
                       :indentation-cache-packages (ref nil))
    :else (comment
            (struct-map :socket-io socket-io
                        :read read-from-socket-io
                        :send send-to-socket-io
                        :serve-requests simple-serve-requests
                        :communication-style style)))))


(defn simple-announce-function [port]
  (when *swank-debug?*
    (. *log-output* println (str "Swank started at port: " port "."))
    (. *log-output* flush)))

(defn slime-secret []
  (try
   ;; We only use this and not slurp because we're lazy about the \n
   (with-open secret (new BufferedReader
                          (new FileReader
                               (str (user-home-path) (. File separator) ".slime-secret")))
     (. secret readLine))
   (catch Throwable e nil)))

(defn accept-authenticated-connection [& args]
  (let [#^Socket new (apply accept-connection args)]
    (try
     (when-let secret (slime-secret)
       (let [first-val (decode-message new)]
         (when-not (= first-val secret)
           (throw (new Exception "Incoming connection doesn't know the password.")))))
     ;; Close connection if something goes wrong
     (catch Throwable e
       (. new close)
       (throw e)))
    new))

(defn serve-requests
  "Read and process all requests on connections"
  ([connection]
     ((connection :serve-requests) connection)))

(defn serve-connection [#^ServerSocket socket style dont-close external-format]
  (try
   (let [client (accept-authenticated-connection socket)]
     (when-not dont-close
       (close-socket socket))
     (let [connection (create-connection client style)]
       ; (run-hook *new-connection-hook* connection)
       (dosync
        (alter *connections* conj connection))
       (serve-requests connection)))
   (finally
    (when-not (or dont-close (. socket isClosed))
      (close-socket socket)))))

(defn setup-server [port announce-fn style dont-close external-format]
  (let [socket (create-socket *loopback-interface* port)
        local-port (local-port socket)]
    (announce-fn local-port)
    (flet [(fn serve []
             (try 
              (serve-connection socket style dont-close external-format)
              (catch java.lang.InterruptedException e nil)))]
      (cond
       (= style :spawn) (spawn (fn []
                                 (cond
                                  dont-close (loop [] (serve) (recur))
                                  :else (serve)))
                               (str "Swank " port))
       ;; fd-handler / sigio not supported
       ;; (one-of? style :fd-handler :sigio) (add-fd-handler socket serve)
       :else (loop [] (when dont-close (serve) (recur)))))
    (dosync
     (alter *listener-sockets* assoc port [style socket]))
    local-port))

(defn announce-server-port [#^String file port]
  (with-open out (new FileWriter file)
    (doto out
      (write (str port "\n"))
      (flush)))
  (simple-announce-function port))

(defn start-server
  "Start the serrver and write the listen port number to
   PORT-FILE. This is the entry point for Emacs."
  ([port-file & options]
     (let [{:keys [style dont-close coding-system]
            :or {style *communication-style*
                 dont-close *dont-close*
                 coding-system *coding-system*}} (apply hash-map options)]
       (setup-server
        0 (fn [port] (announce-server-port port-file port))
        style dont-close
        ;; todo - real external format support
        coding-system))))

(defn create-server [& options]
  (let [{:keys [port style dont-close coding-system]
         :or {port *default-server-port*
              style *communication-style*
              dont-close *dont-close*
              coding-system *coding-system*}} (apply hash-map options)]
    (setup-server port simple-announce-function style dont-close coding-system)))


(defn stop-server
  "Stops a server running on port"
  ([port]
     (let [[style #^ServerSocket socket] (*listener-sockets* port)]
       (cond
        (= style :spawn) (do (. (find-thread-by-name (str "Swank " port)) interrupt)
                             (. socket close)
                             (dosync (alter *listener-sockets* dissoc port)))
        :else nil))))

(defn restart-server
  "Stop the server listening on port, then start a new swank server on
   port running in style. If dont-close is true, then the listen
   socket will accept multiple connections, otherwise it will be
   closed after the first."
  ([& options]
     (let [{:keys [port] :or {port *default-server-port*}} (apply hash-map options)]
       (stop-server port)
       (sleep 5)
       (apply create-server options))))


;;;; Swank functions!

(def *buffer-package* nil)
(def *pending-continuations* nil)

(defn maybe-ns [package]
  (cond
   (symbol? package) (or (find-ns package) (maybe-ns 'user))
   (string? package) (maybe-ns (symbol package))
   (keyword? package) (maybe-ns (name package))
   (instance? clojure.lang.Namespace package) package
   :else (maybe-ns 'user)))

(defn guess-package
  "Guess which package corresponds to string. Return nil if no
   matches."
  ([string]
     (maybe-ns string)))

(defn guess-buffer-package
  "Return a package for a string. Fall back to current if no package
   exists."
  ([string]
     (or (and string (guess-package string))
         *ns*)) )

(comment
 (gen-and-load-class 'swank.DebugQuitException :extends Exception)
 (def *debug-quit* (new swank.DebugQuitException "debug quit"))


 (defn invoke-debugger []
   (try
    (loop [] (read-from-emacs) (recur))
    (catch swank.DebugQuitException dqe
      nil))))

(def *debug-quit* (new Throwable "debug quit"))

(defn invoke-debugger []
  (try
   (loop [] (read-from-emacs) (recur))
   (catch Throwable t
     (when-not (= *debug-quit* t)
       (throw t)))))

;; Disable a few commands as they are not supported
;;      (swank-clojure-debug may redefine these functions)
(defn backtrace [start end] nil)
(defn frame-catch-tags-for-emacs [n] nil)
(defn frame-locals-for-emacs [n] nil)

(defn root-cause
  ([#^Throwable ex]
     (if-let cause (. ex getCause)
       (recur cause)
       ex))
  {:tag Throwable})

(defn spawn-debugger
  "Creates a thread that handles debugging of given exception/thread"
  ([connection id #^Thread th #^Throwable ex]
     (spawn
      (fn []
        (binding [*emacs-connection* connection]
          (let [ex (or (.getCause ex) ex); (root-cause ex) ;; (or (. ex getCause) ex)
                level 1
                message (list (or (. ex getMessage) "No message.") (str "  [Thrown " (class ex) "]") nil)
                options '(("ABORT" "Return to SLIME's top level."))
                error-stack (map list
                                 (iterate inc 0)
                                 (map str (. ex getStackTrace)))
                continuations (list id)]
            (send-to-emacs (list :debug (current-thread) level message options error-stack continuations))
            (send-to-emacs (list :debug-activate (current-thread) level true))
            (invoke-debugger)
            (send-to-emacs (list :debug-return (current-thread) level nil)))))
      (str "Swank Debugger for " (. th getName)))))

(defn eval-for-emacs
  "Bind *buffer-package* to buffer-package and evaluate form. Return
   the result to the id (continuation). Errors are trapped and invoke
   our non-existent debugger. "
  ([form buffer-package id]
     (use-uncaught-exception-handler
      (let [connection *emacs-connection*]
        (fn [th ex]
          (spawn-debugger connection id th ex))))
     (try
      (binding [*buffer-package* buffer-package
                *pending-continuations* (cons id *pending-continuations*)]
        (if (resolve (first form))
          (try
           (let [new-form (map #(if (= % 't) true %) form)
                 ;; new replaces the symbol/variable t with true for CL compatibility
                 result (eval new-form)]
             (run-hook *pre-reply-hook*)
             (send-to-emacs `(:return ~(thread-name (current-thread))
                                      (:ok ~result)
                                      ~id)))
           (catch Throwable t
             (send-to-emacs `(:return ~(thread-name (current-thread))
                                      (:abort)
                                      ~id))
             (throw t)))
          (send-to-emacs `(:return ~(thread-name (current-thread))
                                   (:abort)
                                   ~id)))))))

(defn debugger-info-for-emacs [start end]
  (list nil nil nil *pending-continuations*))

(defn connection-info []
  `(:pid ~(get-pid)
    :style ~(*emacs-connection* :communication-style)
    :lisp-implementation (:type "clojure")
    :package (:name ~(str (ns-name *ns*))
              :prompt ~(str (ns-name *ns*)))
    :version ~(deref *protocol-version*)))

(defn quit-lisp []
  (. System exit 0))

;;;; Evaluation

(defmacro with-buffer-syntax [& body]
  `(binding [*ns* (maybe-ns *buffer-package*)]
     ~@body))

(defn from-string
  "Read a string from inside the *buffer-package*"
  ([string] (with-buffer-syntax (read-from-string string))))

(def format-values-for-echo-area #'pr-str)

(defn eval-region
  "Evaluate string, return the results of the last form as a list and
   a secondary value the last form."
  ([string]
     (with-open rdr (new LineNumberingPushbackReader (new StringReader string))
       (loop [form (read rdr false rdr), value nil, last-form nil]
         (if (= form rdr)
           [value last-form]
           (recur (read rdr false rdr)
                  (eval form)
                  form))))))

(defn interactive-eval [string]
  (with-buffer-syntax
   (let [result (eval (from-string string))]
     (format-values-for-echo-area result))))

(defn interactive-eval-region [string]
  (with-buffer-syntax
   (format-values-for-echo-area (first (eval-region string)))))

(defn track-package [f]
  (let [last-ns *ns*]
    (try
     (f)
     (finally
      (when-not (= last-ns *ns*)
        (send-to-emacs `(:new-package ~(str (ns-name *ns*)) ~(str (ns-name *ns*)))))))))

(defn send-repl-results-to-emacs [val]
  (send-to-emacs `(:write-string ~(str (pr-str val) "\n") :repl-result)))

(def *send-repl-results-function* #'send-repl-results-to-emacs)
(defn repl-eval [string]
  (with-buffer-syntax
   (track-package
    (fn []
      (let [[values last-form] (eval-region string)]
        (*send-repl-results-function* values))))))

(def *listener-eval-function* #'repl-eval)
(defn listener-eval [string]
  (*listener-eval-function* string))

(defn load-file [file-name]
  (pr-str (clojure/load-file file-name)))

(defn compiler-notes-for-emacs []
  ;; todo - not implemented...
  nil)

(defn compile-file-for-emacs [file-name load?]
  (when load?
    (with-buffer-syntax
     (let [start (. System (nanoTime))
           ret (load-file file-name)]
       (list :swank-compilation-unit
             nil
             '(t)
             (list (/ (- (. System (nanoTime)) start) 1000000000.0)))))))


;;;; Simple arglist display
(defn operator-arglist [name package]
  (try
   (let [f (from-string name)]
     (cond
      (keyword? f) "[map]"
      (symbol? f) (let [var (ns-resolve (maybe-ns package) f)]
                    (if-let args (and var (:arglists (meta var)))
                      (pr-str args)
                      nil))
      :else nil))
   (catch Throwable t nil)))

;;;; Simple completions

(defn vars-start-with
  "Runs through the provided vars and collects a list of the names
   that start with a given pattern."
  ([#^String prefix vars]
     (filter (fn [#^String s]
               (and s (not= 0 (. s length)) (. s startsWith prefix)))
             (map (comp str :name meta) vars))))

(defn common-prefix
  "Returns the largest common prefix"
  ([#^String a #^String b]
     (let [limit (min (count a) (count b))]
       (loop [i 0]
         (cond
          (or (= i limit) (not= (. a (charAt i)) (. b (charAt i)))) (. a (substring 0 i))
          :else (recur (inc i))))))
  {:tag String})

(defn symbol-name-parts
  ([symbol]
     (symbol-name-parts symbol nil))
  ([#^String symbol default-ns]
     (let [ns-pos (. symbol (indexOf (int \/)))]
       (if (< ns-pos 0)
         [default-ns symbol]
         [(. symbol (substring 0 ns-pos))
          (. symbol (substring (+ ns-pos 1)))]))))

(defn simple-completions [string package]
  (try
   (let [[sym-ns sym-name] (symbol-name-parts string)
         ns (if sym-ns (find-ns (symbol sym-ns)) (maybe-ns package))
         vars (vals (if sym-ns (ns-publics ns) (ns-map ns)))
         matches (sort (vars-start-with sym-name vars))]
     (if sym-ns
       (list (map (partial str sym-ns "/") matches)
             (if matches
               (str sym-ns "/" (reduce common-prefix matches))
               string))
       (list matches
             (if matches
               (reduce common-prefix matches)
               string))))
   (catch java.lang.Throwable t
     (list nil string))))

;;;; Macroexpansion

(defn apply-macro-expander [expander string]
  (with-buffer-syntax
   (pr-str (expander (from-string string)))))

(defn swank-macroexpand-1 [string]
  (apply-macro-expander macroexpand-1 string))

(defn swank-macroexpand [string]
  (apply-macro-expander macroexpand string))

;; not implemented yet, needs walker
(defn swank-macroexpand-all [string]
  (apply-macro-expander macroexpand string))

;;;; Packages
(defn list-all-package-names
  ([] (map (comp str ns-name) (all-ns)))
  ([nicknames?] (list-all-package-names)))

(defn set-package [name]
  (let [ns (maybe-ns name)]
    (in-ns (ns-name ns))
    (list (str (ns-name ns))
          (str (ns-name ns)))))

;;;; Describe
(defn describe-to-string [var]
  (with-out-str
   (print-doc var)))

(defn describe-symbol [symbol-name]
  (with-buffer-syntax
   (if-let v (resolve (symbol symbol-name))
     (describe-to-string v)
     (str "Unknown symbol " symbol-name))))

(def describe-function #'describe-symbol)

;; Only one namespace... so no kinds
(defn describe-definition-for-emacs [name kind]
  (describe-symbol name))

;; Only one namespace... so only describe symbol
(defn documentation-symbol
  ([symbol-name default] (documentation-symbol))
  ([symbol-name]
     (describe-symbol symbol-name)))


;;;; Source Locations

(defn set-default-directory [directory & ignore]
  ;; incomplete solution, will change search path for find-definitions
  ;; but will not work for load-file etc.
  (. System (setProperty "user.dir" directory))
  directory)

(defn slime-find-file-in-dir [#^File file #^String dir]
  (let [file-name (. file (getPath))
        child (new File (new File dir) file-name)]
    (or (when (. child (exists))
          `(:file ~(. child (getPath))))
        (try
         (let [zipfile (new ZipFile dir)]
           (when (. zipfile (getEntry file-name))
             `(:zip ~dir ~file-name)))
         (catch java.lang.Throwable e false)))))

(defn slime-find-file-in-paths [#^String file paths]
  (let [f (new File file)]
    (if (. f (isAbsolute))
      `(:file ~file)
      (first (filter identity (map (partial slime-find-file-in-dir f) paths))))))

(defn get-path-prop [prop]
  (seq (.. System
           (getProperty prop)
           (split (. File pathSeparator)))))

(defn slime-search-paths []
  (concat (get-path-prop "user.dir")
          (get-path-prop "java.class.path")
          (get-path-prop "sun.boot.class.path")))

(defn find-definitions-for-emacs [name]
  (let [sym-name (from-string name)
        metas (map meta (vals (ns-map (maybe-ns *buffer-package*))))
        definition
        (fn definition [meta]
          (if-let path (slime-find-file-in-paths (:file meta) (slime-search-paths))
            `(~(str "(defn " (:name meta) ")")
              (:location
               ~path
               (:line ~(:line meta))
               nil))
            `(~(str (:name meta))
              (:error "Source definition not found."))))]
    (map definition (filter #(= (:name %) sym-name) metas))))

;;;; Indentation & indentation cache

(defn need-full-indentation-update?
  "Return true if the whole indentation cache should be updated. This
   is a heuristic to avoid scanning all symbols all the time: instead,
   we only do a full scan if the set of packages as changed."
  ([connection]
     (not= (hash (all-ns)) (hash @(connection :indentation-cache-packages)))))

(defn var-indentation [var]
  (flet [(fn indent-loc [meta]
           (or (meta :indent)
               (when-let arglists (:arglists meta)
                 (let [arglist (apply min-key #(or (position '& %1) 0) arglists)
                       amp (position '& arglist)
                       body (position 'body arglist)]
                   (when (and body amp
                              (= (- body amp) 1))
                     amp)))))
         (fn indent-cons [meta]
           (when-let indent-to (indent-loc meta)
             (when (>= indent-to 0)
               `(~(str (:name meta)) . ~indent-to))))]
    (indent-cons (meta var))))

(defn var-indentations-for [nss]
  (filter identity
          (map (comp swank/var-indentation val)
               (filter (comp var? val) (mapcat ns-map nss)))))

(defn every-other [coll]
  (when coll
    (lazy-cons
     (first coll)
     (every-other (drop 2 coll)))))

(defn update-indentation-delta
  "Update the cache and return the changes in a (symbol '. indent) list.
   If FORCE is true then check all symbols, otherwise only check
   symbols belonging to the buffer package"
  ([cache force]
     (let [cache-val @cache]
       (flet [(fn in-cache? [[sym var]]
                (let [indent (var-indentation var)]
                  (when indent
                    (when-not (= (cache-val sym) indent)
                      (list sym indent)))))
              (fn considerations-for [nss]
                (let [vars (filter (comp var? val) (mapcat ns-map nss))]
                  (mapcat in-cache? vars)))]
         (if force
           (when-let updates (considerations-for (all-ns))
             (dosync (apply alter cache assoc updates))
             (every-other (rest updates)))
           (let [ns (maybe-ns *buffer-package*)
                 in-ns? (fn [[sym var]] (and (var? var) (= ns ((meta var) :ns))))]
             (when ns
               (when-let updates (filter identity (considerations-for (list ns)))
                 (dosync (apply alter cache assoc updates))
                 (every-other (rest updates))))))))))

(defn perform-indentation-update
  "Update the indentation cache in connection and update emacs.
   If force is true, then start again without considering the old cache."
  ([connection force]
     (let [cache (connection :indentation-cache)]
       (comment ;; I don't know why real swank wants to blow these away, it shouldn't matter
        (when force
          (dosync (ref-set cache {}))))
       (let [delta (update-indentation-delta cache force)]
         (dosync
          (ref-set (connection :indentation-cache-packages) (hash (all-ns)))
          (when delta
            (send-to-emacs `(:indentation-update ~delta))))))))

(defn sync-indentation-to-emacs
  "Send any indentation updates to Emacs via emacs-connection"
  ([]
     (when *configure-emacs-indentation*
       (let [full? (need-full-indentation-update? *emacs-connection*)]
         (perform-indentation-update *emacs-connection* full?)))))

(add-hook *pre-reply-hook* #'sync-indentation-to-emacs)


;;;; source file cache (not needed)
(defn buffer-first-change [& ignore] nil)

;;;; Not really a debugger
(defn throw-to-toplevel []
  (throw *debug-quit*))

(defn invoke-nth-restart-for-emacs [level n]
  (throw *debug-quit*))



;;;; Inspection (basic clojure aware inspection)

;; This a mess, I'll clean up this code after I figure out exactly
;; what I need for debugging support.

(def *inspectee* (ref nil))
(def *inspectee-content* (ref nil))
(def *inspectee-parts* (ref nil))
(def *inspectee-actions* (ref nil))
(def *inspector-stack* (ref nil))
(def *inspector-history* (ref nil))

(defn reset-inspector []
  (dosync
   (ref-set *inspectee* nil)
   (ref-set *inspectee-content* nil)
   (ref-set *inspectee-parts* [])
   (ref-set *inspectee-actions* [])
   (ref-set *inspector-stack* nil)
   (ref-set *inspector-history* [])))

(defn inspectee-title [obj]
  (cond
   (instance? clojure.lang.LazySeq obj) (str "clojure.lang.LazySeq@...")
   :else (str obj)))

(defn print-part-to-string [value]
  (let [s (inspectee-title value)
        pos (position value @*inspector-history*)]
    (if pos
      (str "#" pos "=" s)
      s)))

(defn assign-index [o dest]
  (dosync
   (let [index (count @dest)]
     (alter dest conj o)
     index)))

(defn value-part [obj s]
  (list :value (or s (print-part-to-string obj))
        (assign-index obj *inspectee-parts*)))

(defn action-part [label lambda refresh?]
  (list :action label
        (assign-index (list lambda refresh?)
                      *inspectee-actions*)))

(defn label-value-line
  ([label value] (label-value-line label value true))
  ([label value newline?]
     (list* (str label) ": " (list :value value)
            (if newline? '((:newline)) nil))))

(defmacro label-value-line* [& label-values]
  `(concat ~@(map (fn [[label value]]
                    `(label-value-line ~label ~value))
                  label-values)))

;; Inspection

;; This is the simple version that only knows about clojure stuff.
;; Many of these will probably be redefined by swank-clojure-debug
(defmulti emacs-inspect
  (fn known-types [obj]
    (cond
     (map? obj) :map
     (vector? obj) :vector
     (var? obj) :var
     (string? obj) :string
     (seq? obj) :seq
     (instance? Class obj) :class
     (instance? clojure.lang.Namespace obj) :namespace)))

(defn inspect-meta-information [obj]
  (when (> (count (meta obj)) 0)
    (concat
     '("Meta Information: " (:newline))
     (mapcat (fn [[key val]]
               `("  " (:value ~key) " = " (:value ~val) (:newline)))
             (meta obj)))))

(defmethod emacs-inspect :map [obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count obj)))
   '("Contents: " (:newline))
   (inspect-meta-information obj)
   (mapcat (fn [[key val]]
             `("  " (:value ~key) " = " (:value ~val)
               (:newline)))
           obj)))

(defmethod emacs-inspect :vector [obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count obj)))
   '("Contents: " (:newline))
   (inspect-meta-information obj)
   (mapcat (fn [i val]
             `(~(str "  " i ". ") (:value ~val) (:newline)))
           (iterate inc 0)
           obj)))

(defmethod emacs-inspect :var [#^clojure.lang.Var obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   (inspect-meta-information obj)
   (when (. obj isBound)
     `("Value: " (:value ~(var-get obj))))))

(defmethod emacs-inspect :string [obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   (inspect-meta-information obj)
   (list (str "Value: " (pr-str obj)))))

(defmethod emacs-inspect :seq [obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   '("Contents: " (:newline))
   (inspect-meta-information obj)
   (mapcat (fn [i val]
             `(~(str "   " i ". ") (:value ~val) (:newline)))
           (iterate inc 0)
           obj)))

(defmethod emacs-inspect :default [obj]
  `("Type: " (:value ~(class obj)) (:newline)
    "Value: " (:value ~(str obj)) (:newline)
    "Don't know how to inspect the object" (:newline)))

(defmethod emacs-inspect :class [#^Class obj]
  (let [meths (. obj getMethods)
        fields (. obj getFields)]
    (concat
     `("Type: " (:value ~(class obj)) (:newline)
       "---" (:newline)
       "Fields: " (:newline))
     (mapcat (fn [f]
               `("  " (:value ~f) (:newline))) fields)
     '("---" (:newline)
       "Methods: " (:newline))
     (mapcat (fn [m]
               `("  " (:value ~m) (:newline))) meths))))


(defn ns-refers-by-ns [#^clojure.lang.Namespace ns]
  (categorize-by (fn [#^clojure.lang.Var v] (. v ns))
                 (map val (ns-refers ns))))

(defmethod emacs-inspect :namespace [#^clojure.lang.Namespace obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count (ns-map obj))))
   '("---" (:newline)
     "Refer from: " (:newline))
   (mapcat (fn [[ns refers]]
             `("  "(:value ~ns) " = " (:value ~refers) (:newline)))
           (ns-refers-by-ns obj))
   (label-value-line*
    ("Imports" (ns-imports obj))
    ("Interns" (ns-interns obj)))))

(defn inspector-content [specs]
  (flet [(fn spec-seq [seq]
           (let [[f & args] seq]
             (cond
              (= f :newline) (str \newline)

              (= f :value)
              (let [[obj & [str]] args]
                (value-part obj str))

              (= f :action)
              (let [[label lambda & options] args
                    {:keys [refresh?]} (apply hash-map options)]
                (action-part label lambda refresh?)))))
         (fn spec-value [val]
           (cond
            (string? val) val
            (seq? val) (spec-seq val)))]
    (map spec-value specs)))

;; Works for infinite sequences, but it lies about length. Luckily, emacs doesn't
;; care.
(defn content-range [lst start end]
    (let [amount-wanted (- end start)
          shifted (drop start lst)
          taken (take amount-wanted shifted)
          amount-taken (count taken)]
      (if (< amount-taken amount-wanted)
        (list taken (+ amount-taken start) start end)
        ;; There's always more until we know there isn't
        (list taken (+ end 500) start end))))

(defn inspect-object [o]
  (dosync
   (ref-set *inspectee* o)
   (alter *inspector-stack* conj o)
   (when-not (filter #(identical? o %) @*inspector-history*)
     (alter *inspector-history* conj o))
   (ref-set *inspectee-content* (inspector-content (emacs-inspect o)))
   (list :title (inspectee-title o)
         :id (assign-index o *inspectee-parts*)
         :content (content-range @*inspectee-content* 0 500))))

(defn init-inspector [string]
  (with-buffer-syntax
   (reset-inspector)
   (inspect-object (eval (read-from-string string)))))

(defn inspect-in-emacs [what]
  (flet [(fn send-it []
           (with-buffer-syntax
            (reset-inspector)
            (send-oob-to-emacs `(:inspect ~(inspect-object what)))))]
    (cond
     *emacs-connection* (send-it)
     (default-connection) (with-connection (default-connection)
                            (send-it)))))

(defn inspector-nth-part [index]
  (get @*inspectee-parts* index))

(defn inspect-nth-part [index]
  (with-buffer-syntax
   (inspect-object (inspector-nth-part index))))

(defn inspector-range [from to]
  (content-range @*inspectee-content* from to))

(defn ref-pop [ref]
  (let [[f & r] @ref]
    (ref-set ref r)
    f))

(defn inspector-call-nth-action [index & args]
  (let [[fn refresh?] (get @*inspectee-actions* index)]
    (apply fn args)
    (if refresh?
      (inspect-object (dosync (ref-pop *inspector-stack*)))
      nil)))

(defn inspector-pop []
  (with-buffer-syntax
   (cond
    (rest @*inspector-stack*)
    (inspect-object
     (dosync
      (ref-pop *inspector-stack*)
      (ref-pop *inspector-stack*)))
    :else nil)))

(defn inspector-next []
  (with-buffer-syntax
   (let [pos (position @*inspectee* @*inspector-history*)]
     (cond
      (= (inc pos) (count @*inspector-history*)) nil
      :else (inspect-object (get @*inspector-history* (inc pos)))))))

(defn inspector-reinspect []
  (inspect-object @*inspectee*))

(defn quit-inspector []
  (reset-inspector)
  nil)

(defn describe-inspectee []
  (with-buffer-syntax
   (str @*inspectee*)))

(comment
  (defn class-exists? [#^String name]
    (try
     (. java.lang.Class forName name)
     (catch Throwable t nil)))
  
  (when (class-exists? "com.sun.jdi.VirtualMachine")
    (swank-require :swank-clojure-debug)))
