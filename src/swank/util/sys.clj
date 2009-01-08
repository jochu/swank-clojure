(ns swank.util.sys)

(defn get-pid
  "Returns the PID of the JVM. This may or may not be accurate
   depending on the JVM in which clojure is running off of."
  ([]
     (or (first (.. java.lang.management.ManagementFactory (getRuntimeMXBean) (getName) (split "@")))
         (System/getProperty "pid")))
  {:tag String})

(defn get-property [name]
  (System/getProperty name))

(defn user-home-path []
  (get-property "user.home"))
