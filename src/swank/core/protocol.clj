(ns swank.core.protocol
  (:use (swank util)
        (swank.util io)))

;; Read forms
(def #^{:private true}
     *namespace-re* #"(^\(:emacs-rex \([a-zA-Z][a-zA-Z0-9]+):")

(defn- fix-namespace
  "Changes the namespace of a function call from pkg:fn to ns/fn. If
   no pkg exists, then nothing is done."
  ([text] (.replaceAll (re-matcher *namespace-re* text) "$1/")))

(defn write-swank-message
  "Given a `writer' (java.io.Writer) and a `message' (typically an
   sexp), encode the message according to the slime protocol and
   write the message into the writer.
   
   The protocol itself is simply a 6-character hex string,
   representing the message length, followed by a lisp-readable
   version of the message itself.

   See also `read-swank-message'."
  ([#^java.io.Writer writer message]
     (let [s   (pr-str message)
           len (.length s)]
       (doto writer
         (.write (format "%06x" len))
         (.write s)
         (.flush))))
  {:tag String})

(def read-fail-exception (Exception. "Error reading swank message"))

(defn read-swank-message
  "Given a `reader' (java.io.Reader), read the message as a clojure
   form (typically a sexp). This method will block until a message is
   completely transfered.

   Note: This function will do some amount of Common Lisp -> clojure
   conversions. This is due to the fact that several slime functions
   like to treat everything it's talking to as a common lisp
   implementation.
     - If an :emacs-rex form is received and the first form contains a
       common lisp package designation, this will convert it to use a
       clojure designation.
     - t will be converted to true

   See also `write-swank-message'."
  ([#^java.io.Reader reader]
     (let [len  (Integer/parseInt (read-chars reader 6 read-fail-exception) 16)
           msg  (read-chars reader len read-fail-exception)
           form (read-string (fix-namespace msg))]
       (if (seq? form)
         (deep-replace {'t true} form)
         form))))
