;;;; swank-clojure.clj --- Swank server for Clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; Instructions for use:
;;;
;;;   1. First you need to have the latest SVN copy of clojure and the
;;;      latest CVS version of slime (this has been untested for any
;;;      other version). (Latest as of 2008-03-27)
;;;
;;;   2. Use a script that will start clojure while providing java a
;;;      pid, name it "clojure", and place it in your PATH. If no pid
;;;      is provided, swank-clojure will fallback on JDK
;;;      implementation specific methods of getting the pid. It may
;;;      not work on other JVMs.
;;;
;;;      For an example of a script that does this, see bin/clojure in
;;;      the git repository at:
;;;         http://clojure.codestuffs.com/pub/clojure-emacs.git
;;;
;;;      If this is not possible, you must edit swank-clojure.el and
;;;      change the slime-lisp-implementations.
;;;
;;;   3. If you haven't already, set the default lisp to load for
;;;      slime via slime-lisp-implementations. If you want the default
;;;      to be clojure, you may skip this step.
;;;
;;;      Example for sbcl:
;;;        (setq slime-lisp-implementations '((sbcl "sbcl")))
;;;
;;;   4. Add to your .emacs
;;;        (add-to-list 'load-path "/path/to/swank-clojure")
;;;        (load "swank-clojure")
;;;
;;;   5. Start slime by:
;;;        M-- M-x slime clojure
;;;
;;;
;;; Note this is a preliminary version that doesn't support several
;;; slime features, but I'm working on it... 
;;;

(clojure/in-ns 'swank)
(clojure/refer 'clojure)

(import '(java.io InputStreamReader PushbackReader StringReader Reader
                  OutputStreamWriter FileWriter Writer
                  File)
        '(clojure.lang LineNumberingPushbackReader)
        '(java.net ServerSocket Socket)
        '(java.util.zip ZipFile)
        ; '(sun.jvmstat.monitor MonitoredHost)
        ; '(com.sun.jdi VirtualMachine)
        )

(def #^String *charset* "iso-8859-1")

;; Utilities

(def *namespace-re* (re-pattern "(^\\(:emacs-rex \\([a-zA-Z][a-zA-Z0-9]+):"))
(defn fix-namespace
  ([text]
     (let [m (re-matcher *namespace-re* text)]
       (. m (replaceAll "$1/")))))

(defn hex->num
  ([hex-str]
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

(defn get-pid []
  (or
   (. System (getProperty "pid"))
   (nth (. (.. java.lang.management.ManagementFactory (getRuntimeMXBean) (getName)) (split "@")) 0)))

(defn position
  "Finds the first position of item within col. Returns nil if not
   found."
  ([item coll]
     (loop [coll coll, i 0]
       (when coll
         (if (= (first coll) item)
           i
           (recur (rest coll) (inc i)))))))

(defn starts-with? [#^String prefix #^String string]
  (. string (startsWith prefix)))

(defn string-empty? [#^String string]
  (= string ""))

(defn current-thread
  ([] (. Thread (currentThread)))
  {:tag Thread})

(defmacro on-thread-do [& body]
  `(on-thread (fn [] ~@body)))

(defn on-thread [#^Runnable f]
  (doto (new Thread f) (start)))

(defmacro if-let*
  ([bindings then]
     `(if-let* ~bindings ~then nil))
  ([bindings then else]
     (if bindings
       `(if-let ~@(take 2 bindings)
            (if-let* ~(drop 2 bindings) ~then ~else)
          ~else)
       then)))

;; Basic Networking

(defn server-new
  "Create a server. If no port is provided, it will choose any free
   port."
  ([] (server-new 0))
  ([port] (new ServerSocket port))
  {:tag ServerSocket})

(defn server-accept
  "Blocks until an incoming request arrives and accepts it"
  ([#^ServerSocket srv] (. srv (accept)))
  {:tag Socket})

(defn server-close
  "Closes a server"
  ([#^ServerSocket srv] (. srv (close))))

(defn read-chars
  "Reads a given amount of characters out of a reader."
  ([#^Reader reader #^Integer num-chars]
     (let [sb (new StringBuilder num-chars)]
       (dotimes count num-chars
         (. sb (append (char (. reader (read))))))
       (str sb)))
  {:tag String})

(defn read-str [str]
  (with-open rdr (new LineNumberingPushbackReader (new StringReader str))
    (read rdr)))

(defn accept-connections
  "Blocks and accepts connections and calls the hander on a socket ."
  ([srv handle]
     (with-open socket (server-accept srv)
       (server-close srv)
       (handle socket))))


;; Slime method utils

(def *slime-fns* (ref {}))

(defn slimefn [name]
  (get @*slime-fns* name))

(defn slime-eval [expr]
  (apply (slimefn (first expr))
         (rest expr)))

(defmacro defslime [name args & body]
  `(sync nil
     (ref-set
      *slime-fns*
      (assoc @*slime-fns* (quote ~name)
             (fn ~name ~args ~@body)))))

(defmacro defslime-unimplemented [& names]
  (let [unimplemented
        (fn unimplemented [name]
          `(defslime ~name [& rest#] nil))]
    `(do ~@(map unimplemented names))))

(defmacro defslime-same [to from]
  `(sync nil
     (ref-set
      *slime-fns*
      (assoc @*slime-fns* (quote ~to)
             (@*slime-fns* (quote ~from))))))

;; Swank packet management

(defn read-length
  "SWANK: Reads the 6 character hex that represents the length of the
   incoming packet."
  ([#^Reader in]
     (let [length (read-chars in 6)]
       (hex->num length))))

(defn read-packet
  "SWANK: Reads a length and packet and parses the packet."
  ([#^Reader in]
     (let [length (read-length in)
           packet (read-chars in length)]
       (read-str (fix-namespace packet)))))

(defn write-packet
  "SWANK: Writes the length and message out"
  ([#^Writer out msg]
     (let [#^String str-msg (pr-str msg)
           #^String str-len (num->hex (count str-msg) 6)]
      (doto out
        (write str-len)
        (write str-msg)
        (flush)))))

(defn write-packets
  "SWANK: Writes several packets out"
  ([#^Writer out & msgs]
     (doseq msg msgs
       (write-packet out msg))))

(def #^Writer *socket-out* nil)

;; clojure reflection
(defn indentation-for [ns]
  (let [indent-loc
        (fn indent-loc [meta]
          (when-let arglists (:arglists meta)
            (let [arglist (apply min-key #(or (position '& %1) 0) arglists)
                  amp (position '& arglist)
                  body (position 'body arglist)]
              (when (and body amp
                         (= (- body amp) 1))
                amp))))
        
        indent-cons
        (fn indent-cons [meta]
          (when-let indent-to (indent-loc meta)
            `(~(str (:name meta)) . ~indent-to)))]

    (filter identity (map (comp indent-cons meta) (vals (ns-map *ns*))))))

;; Swank interface methods

(def *emacs-ns* nil)
(def *emacs-thread* nil)
(def *emacs-id* nil)

(defn throwable-stack-trace [#^Throwable t]
  (with-out-str
   (. t (printStackTrace (new java.io.PrintWriter *out*)))))

(defn swank-writer [#^Writer out]
  (proxy [Writer] []
    ))

(defn write-string [string]
  (write-packet
   *socket-out*
   `(:write-string ~(str string "\n"))))

(defn emacs-rex [sexp package thread id & stuff]
  (println "erex recv'd" (pr-str sexp))
  (flush)
  
  (binding [*emacs-ns* (create-ns (symbol (or package "user")))
            *emacs-thread* thread
            *emacs-id* id]
    (let [sym (first sexp),
          args (rest sexp)]
      (if-let sfn (slimefn (symbol (name sym)))
        (try
         (write-packet *socket-out* `(:return (:ok ~(apply sfn args)) ~*emacs-id*))
         (catch java.lang.Throwable e
           (println e)
           (flush)
           (write-string (throwable-stack-trace e))
           (write-packet *socket-out* `(:return (:abort) ~*emacs-id*))))
        (do
          (write-packet *socket-out* `(:return (:ok nil) ~*emacs-id*))
          (println "encountered unknown:" sexp)
          (flush))))))

(defn dispatch [packet]
  (println "recv'd packet" (pr-str packet))
  (flush)
  (when (seq? packet)
    (let [type (first packet)]
      (when (= type :emacs-rex)
        (apply emacs-rex (rest packet))))))

(defn write-port-file [#^Integer port #^String file]
  (with-open port-file (new FileWriter file)
    (. port-file (write (str port "\n")))))

(defn swank-handler [#^Socket socket]
  (let [socket-in (new InputStreamReader (. socket (getInputStream)) *charset*)]
    (binding [*socket-out* (new OutputStreamWriter (. socket (getOutputStream)) *charset*)]
      (loop [packet (read-packet socket-in)]
        (dispatch packet)
        (recur (read-packet socket-in))))))

(defn start-swank
  "SWANK: Create a swank server and write port to port-file"
  ([port-file]
     (let [srv (server-new)
           local-port (. srv (getLocalPort))]
       (write-port-file local-port port-file)
       (accept-connections srv swank-handler))))

(defn swank
  ([] (swank 4005))
  ([port]
     (let [srv (server-new port)]
       (accept-connections srv swank-handler))))


;; slime methods

(defslime connection-info [& ignore]
  (write-packets
   *socket-out*
   `(:indentation-update
     ~(indentation-for (create-ns 'user))))
  `(:pid ~(get-pid)
     :package (:name ~(str (ns-name *emacs-ns*))
               :prompt ~(str (ns-name *emacs-ns*)))
     :lisp-implementation (:type "clojure")
     :version "2008-03-27"))


(defn slime-eval [sexp]
  (binding [*ns* *emacs-ns*]
    (let [result (clojure/eval sexp)]
      (when (not= *ns* *emacs-ns*)
        (let [new-ns-name (str (ns-name *ns*))]
          (write-packet
           *socket-out*
           `(:new-package ~new-ns-name ~new-ns-name))))
      result)))

(defslime listener-eval [sexp & ignore]
  (let [result (slime-eval (read-str sexp))]
    `(:values ~(pr-str result))))

(defslime operator-arglist [sym namespace & ignore]
  (if-let* [ns-name (or namespace "user")
            the-ns (find-ns (symbol ns-name))
            the-sym (ns-resolve the-ns (symbol sym))
            the-args (:arglists (meta the-sym))]
     (pr-str the-args)))

(defslime load-file [file-name & ignore]
  (let [load (clojure/load-file file-name)]
    (str "Loaded: " file-name " => " load)))

(defslime compile-file-for-emacs [file-name load? & ignore]
  (when load?
    ((slimefn 'load-file) file-name)
    ;; incomplete, need return time taken to load
    (list "T" "0")))

(defslime interactive-eval [sexp & ignore]
  (let [result (slime-eval (read-str sexp))]
    (str "=> " (pr-str result))))

(defn vars-start-with
  "Runs through the provided vars and collects a list of the names
   that start with a given pattern."
  ([prefix vars]
     (filter #(and % (not (string-empty? %)) (starts-with? prefix %))
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
     (symbol-name-parts symbol "user"))
  ([#^String symbol default-ns]
     (let [ns-pos (. symbol (indexOf (int \/)))]
       (if (< ns-pos 0)
         [default-ns symbol]
         [(. symbol (substring 0 ns-pos))
          (. symbol (substring (+ ns-pos 1)))]))))

(defslime simple-completions [pattern ns & ignore]
  (try
   (let [ns-name (eval ns)
         [sym-ns sym-name] (symbol-name-parts pattern nil)
         ns (or (when sym-ns (create-ns (symbol sym-ns)))
                (when ns-name (create-ns (symbol ns-name)))
                *emacs-ns*)
         publics? (not= ns *emacs-ns*)
         vars (vals (if publics? (ns-map ns) (ns-publics ns)))
         matches (sort (vars-start-with sym-name vars))]
     (if sym-ns
       (list (map (partial str sym-ns "/") matches)
             (if matches
               (str sym-ns "/" (reduce common-prefix matches))
               pattern))
       (list matches
             (if matches
               (reduce common-prefix matches)
               pattern))))
   (catch java.lang.Throwable t
     (list nil pattern))))

(defslime swank-macroexpand-1 [sexp & ignore]
  (let [s (read-str sexp)]
    (if (seq? s)
      (pr-str (macroexpand-1 s))
      sexp)))

(defslime swank-macroexpand [sexp & ignore]
  (let [s (read-str sexp)]
    (if (seq? s)
     (pr-str (macroexpand s))
     sexp)))

;; doesn't quite do what it's supposed to... but it's here for now
(defslime-same swank-macroexpand-all swank-macroexpand)

(defslime quit-lisp [& ignore]
  (. System (exit 0)))

(defslime describe-function [name & ignore]
  (if-let var (ns-resolve *emacs-ns* (symbol name))
    (with-out-str
     (print-doc var))
    "Unknown symbol"))

(defslime-same describe-symbol describe-function)

(defslime list-all-package-names [nicknames? & ignore]
  (map (comp str ns-name) (all-ns)))

(defslime set-package [ns & ignore]
  (if (some #(= ns %) (map (comp str ns-name) (all-ns)))
    (list ns ns)
    (throw (new Exception (str "Invalid NS " ns)))))

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

(defslime find-definitions-for-emacs [name & ignore]
  (let [sym-name (symbol name)
        vars (vals (ns-map *emacs-ns*))
        metas (map meta vars)
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


(defslime set-default-directory [directory & ignore]
  ;; incomplete solution, will change search path for find-definitions
  ;; but will not work for load-file etc.
  (. System (setProperty "user.dir" directory))
  directory)

(defslime quit-lisp [& ignore]
  (. System (exit 0)))

(defslime-unimplemented
  compiler-notes-for-emacs
  buffer-first-change
  swank-require)

"Go swank!"