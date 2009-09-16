(ns swank.test-swank.core.protocol
  (:import (java.io StringReader
                    StringWriter))
  (:use clojure.test
        swank.core.protocol))

;; currently here until test-is 
(deftest reading-messages
  (are [msg form] (with-open [reader (StringReader. msg)]
                    (= (read-swank-message reader) form))
       "0000017"                        7
       "000013(:keyword \"string\")"    '(:keyword "string")
       "000018(nested (list [vector]))" '(nested (list [vector]))))

(deftest writing-messages
  (are [form msg] (with-open [writer (StringWriter.)]
                    (write-swank-message writer form)
                    (= (.toString writer) msg))
       
       9                         "0000019"
       '(:keyword "string")      "000013(:keyword \"string\")"
       '(nested (list [vector])) "000018(nested (list [vector]))"))