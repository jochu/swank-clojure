(ns swank.util.java)

(defn method-name [#^java.lang.reflect.Method method]
  (.getName method))

(defn field-name [#^java.lang.reflect.Field field]
  (.getName field))

(defn method-static? [#^java.lang.reflect.Method method]
  (java.lang.reflect.Modifier/isStatic (.getModifiers method)))

(defn field-static? [#^java.lang.reflect.Field field]
  (java.lang.reflect.Modifier/isStatic (.getModifiers field)))

(defn static-methods [#^Class class]
  (filter method-static? (.getMethods class)))

(defn static-fields [#^Class class]
  (filter field-static? (.getDeclaredFields class)))

(defn instance-methods [#^Class class]
  (remove method-static? (.getMethods class)))
