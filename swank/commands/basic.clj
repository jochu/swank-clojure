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
     (eval-region string "NO_SOURCE_FILE" 1))
  ([string file line]
     (with-open [rdr (proxy [LineNumberingPushbackReader] ((StringReader. string))
                       (getLineNumber [] line))]
       (binding [*file* file]
         (loop [form (read rdr false rdr), value nil, last-form nil]
           (if (= form rdr)
             [value last-form]
             (recur (read rdr false rdr)
                    (eval form)
                    form)))))))

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
  (pretty-pr-code (expander (read-string string))))

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

(defn- line-at-position [file position]
  (try
   (with-open [f (java.io.LineNumberReader. (java.io.FileReader. file))]
     (.skip f position)
     (.getLineNumber f))
   (catch Exception e 1)))

(defslimefn compile-string-for-emacs [string buffer position directory debug]
  (let [start (System/nanoTime)
        line (line-at-position directory position)
        ret (with-emacs-package (eval-region string directory line))
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

;;;; Documentation

(defn- briefly-describe-symbol-for-emacs [var]
  (let [lines (fn [s] (seq (.split #^String s (System/getProperty "line.separator"))))
        [_ symbol-name arglists d1 d2 & __] (lines (describe-to-string var))
        macro? (= d1 "Macro")]
    (list :designator symbol-name
          (cond
            macro? :macro
            (:arglists ^var) :function
            :else :variable)
          (apply str (concat arglists (if macro? d2 d1))))))

(defn- make-apropos-matcher [pattern case-sensitive?]
  (let [pattern (java.util.regex.Pattern/quote pattern)
        pat (re-pattern (if case-sensitive?
                          pattern
                          (format "(?i:%s)" pattern)))]
    (fn [var] (re-find pat (pr-str var)))))

(defn- apropos-symbols [string external-only? case-sensitive? package]
  (let [packages (or (when package [package]) (all-ns))
        matcher (make-apropos-matcher string case-sensitive?)
        lister (if external-only? ns-publics ns-interns)]
    (filter matcher
            (apply concat (map (comp (partial map second) lister)
                               packages)))))

(defn- present-symbol-before
  "Comparator such that x belongs before y in a printed summary of symbols.
Sorted alphabetically by namespace name and then symbol name, except
that symbols accessible in the current namespace go first."
  [x y]
  (let [accessible?
        (fn [var] (= (ns-resolve (maybe-ns *current-package*) (:name ^var))
                     var))
        ax (accessible? x) ay (accessible? y)]
    (cond
      (and ax ay) (compare (:name ^x) (:name ^y))
      ax -1
      ay 1
      :else (let [nx (str (:ns ^x)) ny (str (:ns ^y))]
              (if (= nx ny)
                (compare (:name ^x) (:name ^y))
                (compare nx ny))))))

(defslimefn apropos-list-for-emacs
  ([name]
     (apropos-list-for-emacs name nil))
  ([name external-only?]
     (apropos-list-for-emacs name external-only? nil))
  ([name external-only? case-sensitive?]
     (apropos-list-for-emacs name external-only? case-sensitive? nil))
  ([name external-only? case-sensitive? package]
     (let [package (when package
                     (or (find-ns (symbol package))
                         'user))]
       (map briefly-describe-symbol-for-emacs
            (sort present-symbol-before
                  (apropos-symbols name external-only? case-sensitive?
                                   package))))))

;;;; Operator messages
(defslimefn operator-arglist [name package]
  (try
   (let [f (read-string name)]
     (cond
      (keyword? f) "([map])"
      (symbol? f) (let [var (ns-resolve (maybe-ns package) f)]
                    (if-let [args (and var (:arglists (meta var)))]
                      (pr-str args)
                      nil))
      :else nil))
   (catch Throwable t nil)))

;;;; Completions

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

(defn- slime-zip-resource [#^java.net.URL resource]
  (let [jar-connection #^java.net.JarURLConnection (.openConnection resource)]
    (list :zip (.getFile (.getJarFileURL jar-connection)) (.getEntryName jar-connection))))

(defn- slime-file-resource [#^java.net.URL resource]
  (list :file (.getFile resource)))

(defn- slime-find-resource [#^String file]
  (let [resource (.getResource (clojure.lang.RT/baseLoader) file)]
    (if (= (.getProtocol resource) "jar")
      (slime-zip-resource resource)
      (slime-file-resource resource))))

(defn- slime-find-file [#^String file]
  (if (.isAbsolute (File. file))
    (list :file file)
    (slime-find-resource file)))

(defn- namespace-to-path [ns]
  (let [#^String ns-str (name (ns-name ns))
        last-dot-index (.lastIndexOf ns-str ".")]
    (-> (if (< 0 last-dot-index) (.substring ns-str 0 last-dot-index) ns-str)
        (.replace \- \_)
        (.replace \. \/))))

(defn source-location-for-frame [#^StackTraceElement frame]
  (let [line     (.getLineNumber frame)
        filename (if (.. frame getFileName (endsWith ".java"))
                   (.. frame getClassName (replace \. \/)
                       (substring 0 (.lastIndexOf (.getClassName frame) "."))
                       (concat (str File/separator (.getFileName frame))))
                   (str (namespace-to-path
                         (symbol ((re-find #"(.*?)\$"
                                           (.getClassName frame)) 1)))
                        File/separator (.getFileName frame)))
        path     (slime-find-file filename)]
    `(:location ~path (:line ~line) nil)))

(defslimefn find-definitions-for-emacs [name]
  (let [sym-name (read-string name)
        sym-var (ns-resolve (maybe-ns *current-package*) sym-name)]
    (when-let [meta (and sym-var (meta sym-var))]
      (if-let [path (slime-find-file (:file meta))]
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

(defslimefn frame-source-location [n]
  (source-location-for-frame
     (nth (.getStackTrace *current-exception*) n)))

;; Older versions of slime use this instead of the above.
(defslimefn frame-source-location-for-emacs [n]
  (source-location-for-frame
     (nth (.getStackTrace *current-exception*) n)))

(defslimefn create-repl [target] '("user" "user"))
