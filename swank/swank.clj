(clojure/create-ns 'swank)
(clojure/ns swank
    (:import (java.io InputStreamReader PushbackReader StringReader Reader
                       BufferedReader FileReader
                       OutputStreamWriter FileWriter Writer StringWriter
                       OutputStream PrintStream File)
             (clojure.lang LineNumberingPushbackReader)
             (java.net ServerSocket Socket InetAddress)
             (java.util.zip ZipFile)))
(clojure/refer 'clojure :exclude '(load-file))

(load-resources "swank-clojure.clj" "swank-c-p-c.clj" "swank-arglists.clj")