(ns swank.commands.contrib.trace
  (:use (swank commands)))

;;;; trace entry/exit functions

(defn trace-entry [id nm args]
  (.println System/err (str "TRACE " id ": " nm " called with " (pr-str args))))

(defn trace-exit [id nm returns]
  (.println System/err (str "TRACE " id ": " nm " returned " (pr-str returns))))

;;;; wrapper to call the tracing functions 

(def
 #^{:doc "Prevents tracing of functions we use for the trace itself"}
 prevent-trace false)

(defn trace-wrap
  [key f]
  (fn [& args]
    (if prevent-trace
      (apply f args)
      (binding [prevent-trace true]
        (let [id (gensym "t")]
          (trace-entry id key args)
          (binding [prevent-trace false]
            (let [v (apply f args)]
              (binding [prevent-trace true]
                (trace-exit id key v))
              v)))))))

;;;; map of trace functions
(defstruct traced-fn-struct :orig :traced)
(defonce traced-map (ref {}))

;;;; enable/disable trace for given function

(defn- enable-trace [key f]
  (let [traced-fn (trace-wrap key f)]
    (println "tracing" key)
    (dosync (alter traced-map assoc key 
                   (struct-map traced-fn-struct :orig f :traced traced-fn)))
    (alter-var-root key (fn [_] traced-fn))))

(defn- disable-trace [key traced-fn]
  (println "untracing" key)
  (alter-var-root key (fn [_] (:orig traced-fn)))
  (dosync (alter traced-map dissoc key)))

(defn trace-aux [f key]
  (when (not (fn? f))
    (throw (Exception. "not a function")))
  (let [traced-fn (@traced-map key)]
    (if (and traced-fn (= f (:traced traced-fn)))
      (do
        (disable-trace key traced-fn)
        (str "Tracing disabled on " key))
      (do
        (enable-trace key f)
        (str "Tracing enabled on " key)))))

(defslimefn untrace-all []
  (doseq [[key traced-fn] @traced-map]
    (when (= (:traced traced-fn) (deref key))
      (disable-trace key traced-fn)))
  (dosync (ref-set traced-map {}))
  "All tracing disabled")

(defslimefn swank-toggle-trace [string]
  (let [sym (resolve (symbol string))]
    (when (nil? sym)
      (throw (Exception. (str "cannot resolve " string))))
    (trace-aux (var-get sym) sym)))
