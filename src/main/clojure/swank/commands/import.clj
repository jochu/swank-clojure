(ns swank.commands.import
  (:use [swank core commands])
  (:import [java.util.regex Pattern]))

(defn find-classes [regex]
  (let [search-path-properities ["java.class.path" "sun.boot.class.path"]]
    (for [search-path search-path-properities
          jar (filter #(.endsWith % ".jar")
                      (.split (System/getProperty search-path)
                              (System/getProperty "path.separator")))
          entry (try (filter #(.endsWith (.getName %) ".class")
                             (enumeration-seq (.entries
                                               (java.util.jar.JarFile. jar))))
                     (catch Exception _))
          name [(.. entry getName (replaceAll "\\.class$" ""))]
          :when (re-find regex name)]
      name)))

(defslimefn classes-for [classname]
  (map #(.replaceAll % "/" ".")
       (find-classes (java.util.regex.Pattern/compile classname))))
