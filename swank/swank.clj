(clojure/ns swank
  (:refer-clojure :exclude [load-file])
  (:import (java.io InputStreamReader PushbackReader StringReader Reader
                    BufferedReader FileReader
                    OutputStreamWriter FileWriter Writer StringWriter
                    OutputStream PrintStream File)
           (clojure.lang LineNumberingPushbackReader)
           (java.net ServerSocket Socket InetAddress)
           (java.util.zip ZipFile)))

(load-resources "swank-clojure.clj" "swank-c-p-c.clj" "swank-arglists.clj")
