(ns swank.util.java)

(defn method-name [#^java.lang.reflect.Method method]
  (.getName method))

(defn method-static? [#^java.lang.reflect.Method method]
  (java.lang.reflect.Modifier/isStatic (.getModifiers method)))

(defn static-methods [#^Class class]
  (filter method-static? (.getMethods class)))

(defn instance-methods [#^Class class]
  (remove method-static? (.getMethods class)))
