(ns swank.clj-contrib.macroexpand)

(def
 #^{:private true}
 walk-enabled?
 (.getResource (clojure.lang.RT/baseLoader) "clojure/contrib/walk.clj"))

(when walk-enabled?
  (require 'clojure.contrib.walk))

(defmacro macroexpand-all* [form]
  (if walk-enabled?
    `(clojure.contrib.walk/macroexpand-all ~form)
    `(macroexpand ~form)))

(defn macroexpand-all [form]
  (macroexpand-all* form))