(ns swank.util.io
  (:use (swank util)
        (swank.util.concurrent thread)))

(defn read-chars [n #^java.io.Reader rdr]
  (let [sb (StringBuilder.)]
    (dotimes [i n]
      (let [c (.read rdr)]
        (when (not= c -1)
          (.append sb (char c)))))
    (str sb)))

(defn call-on-flush-stream
  "Creates a stream that will call a given function when flushed."
  ([flushf]
     (let [closed? (ref nil)
           
           #^java.io.StringWriter
           stream (proxy [java.io.StringWriter] []
                    (close [] (dosync (ref-set closed? true)))
                    (flush []
                      (let [#^java.io.StringWriter me this] ;; only so it know what it is
                        (let [len (.. me getBuffer length)]
                          (when (> len 0)
                            (flushf (.. me getBuffer (substring 0 len)))
                            (.. me getBuffer (delete 0 len)))))))]

       (dothread
         (thread-set-name "Call-on-write Stream")
         (continuously
           (Thread/sleep 200)
           (when-not @closed?
             (.flush stream))))
       
       stream))
  {:tag java.io.StringWriter})