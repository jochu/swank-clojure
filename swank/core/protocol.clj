(ns swank.core.protocol
  (:use (swank util)
        (swank.util io)))

;; Read forms
(def #^{:private true}
     *namespace-re* (re-pattern "(^\\(:emacs-rex \\([a-zA-Z][a-zA-Z0-9]+):"))
(defn- fix-namespace
  "Changes the namespace of a function call from pkg:fn to ns/fn. If
   no pkg exists, then nothing is done."
  ([text] (.replaceAll (re-matcher *namespace-re* text) "$1/")))

(defn- hex->num
  "Converts a hex string into an integer"
  ([#^String hex-str] (Integer/parseInt hex-str 16))
  {:tag Integer})

(defn- num->hex
  "Converts a number to a hex string. If a minimum length is provided,
   the hex number will be left padded with 0s."
  ([num]
     (Integer/toHexString num))
  ([num min-len]
     (let [hex (num->hex num)
           len (count hex)]
       (if (< len min-len)
         (str (apply str (replicate (- min-len len) \0)) hex)
         hex)))
  {:tag String})

(defn write-swank-message
  "Encodes a message into slime encoded message"
  ([#^java.io.Writer w message]
     (let [s (pr-str message)
           len (.length s)]
       (doto w
         (.write (num->hex len 6))
         (.write s)
         (.flush))))
  {:tag String})

(defn read-swank-message
  "Decodes a message from emacs. Expects a reader."
  ([#^java.io.Reader rdr]
     (let [len (hex->num (read-chars 6 rdr))
           msg (read-chars len rdr)
           form (read-from-string (fix-namespace msg))]
       (if (seq? form)
         (deep-replace {'t true} form)
         form))))

