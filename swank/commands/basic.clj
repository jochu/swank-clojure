(ns swank.commands.basic
  (:refer-clojure :exclude [load-file])
  (:use (swank util commands core)
        (swank.util.concurrent thread)
        (swank.util string clojure)
        (swank.clj-contrib pprint macroexpand))
  (:require (swank.util [sys :as sys]))
  (:import (java.io StringReader File)
           (java.util.zip ZipFile)
           (clojure.lang LineNumberingPushbackReader)))

;;;; Connection

(defslimefn connection-info []
  `(:pid ~(sys/get-pid)
    :style :spawn
    :lisp-implementation (:type "clojure" :name "clojure")
    :package (:name ~(name (ns-name *ns*))
              :prompt ~(name (ns-name *ns*)))
    :version ~(deref *protocol-version*)))

(defslimefn quit-lisp []
  (System/exit 0))

;;;; Evaluation

(defn- eval-region
  "Evaluate string, return the results of the last form as a list and
   a secondary value the last form."
  ([string]
     (with-open [rdr (LineNumberingPushbackReader. (StringReader. string))]
       (loop [form (read rdr false rdr), value nil, last-form nil]
         (if (= form rdr)
           [value last-form]
           (recur (read rdr false rdr)
                  (eval form)
                  form))))))

(defslimefn interactive-eval-region [string]
  (with-emacs-package
   (pr-str (first (eval-region string)))))

(defslimefn interactive-eval [string]
  (with-emacs-package
    (pr-str (first (eval-region string)))))

(defslimefn listener-eval [form]
  (with-emacs-package
   (with-package-tracking
    (let [[value last-form] (eval-region form)]
      (when (and last-form (not (one-of? last-form '*1 '*2 '*3 '*e)))
        (set! *3 *2)
        (set! *2 *1)
        (set! *1 value))
      (send-repl-results-to-emacs value)))))

(defslimefn eval-and-grab-output [string]
  (with-emacs-package
    (with-local-vars [retval nil]
      (list (with-out-str
              (var-set retval (pr-str (first (eval-region string)))))
            (var-get retval)))))

;;;; Macro expansion

(defn- apply-macro-expander [expander string]
  (pretty-pr-code (expander (read-from-string string))))

(defslimefn swank-macroexpand-1 [string]
  (apply-macro-expander macroexpand-1 string))

(defslimefn swank-macroexpand [string]
  (apply-macro-expander macroexpand string))

;; not implemented yet, needs walker
(defslimefn swank-macroexpand-all [string]
  (apply-macro-expander macroexpand-all string))

;;;; Compiler / Execution

(def *compiler-exception-location-re* #"^clojure\\.lang\\.Compiler\\$CompilerException: ([^:]+):([^:]+):")
(defn- guess-compiler-exception-location [#^Throwable t]
  (when (instance? clojure.lang.Compiler$CompilerException t)
    (let [[match file line] (re-find *compiler-exception-location-re* (.toString t))]
      (when (and file line)
        `(:location (:file ~file) (:line ~(Integer/parseInt line)) nil)))))

;; TODO: Make more and better guesses
(defn- exception-location [#^Throwable t]
  (or (guess-compiler-exception-location t)
      '(:error "No error location available")))

;; plist of message, severity, location, references, short-message
(defn- exception-to-message [#^Throwable t]
  `(:message ~(.toString t)
             :severity :error
             :location ~(exception-location t) 
             :references nil
             :short-message ~(.toString t)))

(defn- compile-file-for-emacs*
  "Compiles a file for emacs. Because clojure doesn't compile, this is
   simple an alias for load file w/ timing and messages. This function
   is to reply with the following:
     (:swank-compilation-unit notes results durations)"
  ([file-name]
     (let [start (System/nanoTime)]
       (try
        (let [ret (clojure.core/load-file file-name)
              delta (- (System/nanoTime) start)]
          `(:compilation-result nil ~(pr-str ret) ~(/ delta 1000000000.0)))
        (catch Throwable t
          (let [delta (- (System/nanoTime) start)
                causes (exception-causes t)
                num (count causes)]
            (.printStackTrace t) ;; prints to *inferior-lisp*
            `(:compilation-result
              ~(map exception-to-message causes) ;; notes
              nil ;; results
              ~(/ delta 1000000000.0) ;; durations
              )))))))

(defslimefn compile-file-for-emacs
  ([file-name load? compile-options]
     (when load?
       (compile-file-for-emacs* file-name))))

(defslimefn load-file [file-name]
  (pr-str (clojure.core/load-file file-name)))

(defslimefn compile-string-for-emacs [string buffer position directory debug]
  (let [start (System/nanoTime)
        ret (with-emacs-package (eval-region string))
        delta (- (System/nanoTime) start)]
    `(:compilation-result nil ~(pr-str ret) ~(/ delta 1000000000.0))))

;;;; Describe

(defn- describe-to-string [var]
  (with-out-str
   (print-doc var)))

(defn- describe-symbol* [symbol-name]
  (with-emacs-package
   (if-let [v (ns-resolve (maybe-ns *current-package*) (symbol symbol-name))]
     (describe-to-string v)
     (str "Unknown symbol " symbol-name))))

(defslimefn describe-symbol [symbol-name]
  (describe-symbol* symbol-name))

(defslimefn describe-function [symbol-name]
  (describe-symbol* symbol-name))

;; Only one namespace... so no kinds
(defslimefn describe-definition-for-emacs [name kind]
  (describe-symbol* name))

;; Only one namespace... so only describe symbol
(defslimefn documentation-symbol
  ([symbol-name default] (documentation-symbol symbol-name))
  ([symbol-name] (describe-symbol* symbol-name)))


;;;; Operator messages
(defslimefn operator-arglist [name package]
  (try
   (let [f (read-from-string name)]
     (cond
      (keyword? f) "([map])"
      (symbol? f) (let [var (ns-resolve (maybe-ns package) f)]
                    (if-let [args (and var (:arglists (meta var)))]
                      (pr-str args)
                      nil))
      :else nil))
   (catch Throwable t nil)))

;;;; Completions

(defn- vars-with-prefix
  "Filters a coll of vars and returns only those that have a given
   prefix."
  ([#^String prefix vars]
     (filter #(.startsWith #^String % prefix) (map (comp name :name meta) vars))))

(defn- maybe-alias [sym ns]
  (or (resolve-ns sym (maybe-ns ns))
      (maybe-ns ns)))

(defslimefn simple-completions [symbol-string package]
  (try
   (let [[sym-ns sym-name] (symbol-name-parts symbol-string)
         ns (if sym-ns (maybe-alias (symbol sym-ns) package) (maybe-ns package))
         vars (if sym-ns (vals (ns-publics ns)) (filter var? (vals (ns-map ns))))
         matches (seq (sort (vars-with-prefix sym-name vars)))]
     (if sym-ns
       (list (map (partial str sym-ns "/") matches)
             (if matches
               (str sym-ns "/" (reduce largest-common-prefix matches))
               symbol-string))
       (list matches
             (if matches
               (reduce largest-common-prefix matches)
               symbol-string))))
   (catch java.lang.Throwable t
     (list nil symbol-string))))


(defslimefn list-all-package-names
  ([] (map (comp str ns-name) (all-ns)))
  ([nicknames?] (list-all-package-names)))

(defslimefn set-package [name]
  (let [ns (maybe-ns name)]
    (in-ns (ns-name ns))
    (list (str (ns-name ns))
          (str (ns-name ns)))))

;;;; Source Locations
(comment
  "Sets the default directory (java's user.dir). Note, however, that
   this will not change the search path of load-file. ")
(defslimefn set-default-directory
  ([directory & ignore]
     (System/setProperty "user.dir" directory)
     directory))


;;;; meta dot find

(defn- slime-find-file-in-dir [#^File file #^String dir]
  (let [file-name (. file (getPath))
        child (File. (File. dir) file-name)]
    (or (when (.exists child)
          `(:file ~(.getPath child)))
        (try
         (let [zipfile (ZipFile. dir)]
           (when (.getEntry zipfile file-name)
             `(:zip ~dir ~file-name)))
         (catch Throwable e false)))))

(defn- slime-find-file-in-paths [#^String file paths]
  (let [f (File. file)]
    (if (.isAbsolute f)
      `(:file ~file)
      (first (filter identity (map #(slime-find-file-in-dir f %) paths))))))

(defn- get-path-prop
  "Returns a coll of the paths represented in a system property"
  ([prop]
     (seq (-> (System/getProperty prop)
              (.split File/pathSeparator))))
  ([prop & props]
     (lazy-cat (get-path-prop prop) (mapcat get-path-prop props))))

(defn- slime-search-paths []
  (concat (get-path-prop "user.dir" "java.class.path" "sun.boot.class.path")
          (let [loader (clojure.lang.RT/baseLoader)]
            (when (instance? java.net.URLClassLoader loader)
              (map #(.getPath #^java.net.URL %)
                   (.getURLs #^java.net.URLClassLoader (cast java.net.URLClassLoader (clojure.lang.RT/baseLoader))))))))

(defn- namespace-to-path [ns]
  (let [#^String ns-str (name (ns-name ns))]
    (-> ns-str
        (.substring 0 (.lastIndexOf ns-str "."))
        (.replace \- \_)
        (.replace \. \/))))

(defn source-location-for-frame [frame]
  (let [line     (.getLineNumber frame)
        frame-ns ((re-find #"(.*?)\$" (.getClassName frame)) 1)
        filename (str (namespace-to-path (symbol frame-ns)) File/separator (.getFileName frame))
        path     (slime-find-file-in-paths filename (slime-search-paths))]
    `(:location ~path (:line ~line) nil)))

(defslimefn find-definitions-for-emacs [name]
  (let [sym-name (read-from-string name)
        sym-var (ns-resolve (maybe-ns *current-package*) sym-name)]
    (when-let [meta (and sym-var (meta sym-var))]
      (if-let [path (or
                     ;; Check first check using full namespace
                     (slime-find-file-in-paths (str (namespace-to-path (:ns meta))
                                                       File/separator
                                                       (:file meta)) (slime-search-paths))
                     ;; Otherwise check using just the filename
                     (slime-find-file-in-paths (:file meta) (slime-search-paths)))]
        `((~(str "(defn " (:name meta) ")")
           (:location
            ~path
            (:line ~(:line meta))
            nil)))
        `((~(str (:name meta))
           (:error "Source definition not found.")))))))


(defslimefn throw-to-toplevel []
  (throw *debug-quit-exception*))

(defslimefn invoke-nth-restart-for-emacs [level n]
  (if (= n 1)
    (let [cause (.getCause *current-exception*)]
      (invoke-debugger cause *debug-thread-id*)
      (.getMessage cause))
    (throw *debug-quit-exception*)))

(defslimefn backtrace [start end]
  (doall (take (- end start) (drop start (exception-stacktrace *current-exception*)))))

(defslimefn buffer-first-change [file-name] nil)

(defslimefn frame-catch-tags-for-emacs [n] nil)
(defslimefn frame-locals-for-emacs [n] nil)

(defslimefn frame-source-location-for-emacs [n]
  (source-location-for-frame
     (nth (.getStackTrace *current-exception*) n)))

(defslimefn create-repl [target] '("user" user))
