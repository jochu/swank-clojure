(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)))

;;; Ported from slime's swank.
(defn- compound-prefix-match
  "Takes a `prefix' and a `target' string and which returns position
  of last character in `target' which matches `prefix' if `prefix' is
  a compound-prefix of `target', and otherwise nil.

  Viewing each of `prefix' and `target' as a series of substrings
  delimited by `delimeter', if each substring of `prefix' is a prefix
  of the corresponding substring in `target' then we call `prefix' a
  compound-prefix of `target'."
  [delimeter,
   #^String prefix,
   #^String target]
  (if (= "" prefix)
    0
    (loop [prefix prefix
           tpos 0]
      (let [ch (first prefix)
            new-tpos (if (= ch delimeter)
                       (position delimeter target tpos)
                       tpos)]
        (when (and tpos
                   (< tpos (.length target))
                   (if (not= tpos new-tpos)
                     new-tpos
                     (= ch (.charAt target tpos))))
          (if-let [newprefix (rest prefix)]
              (recur newprefix
                     (inc new-tpos))
            new-tpos))))))

(defn- symbol-name-parts
  "Parses a symbol name into a namespace and a name. If name doesn't
   contain a namespace, the default-ns is used (nil if none provided)."
  [#^String symbol]
  (let [ns-pos (.indexOf symbol (int \/))]
    (cond
     (= ns-pos -1) ;; no slash?
       [nil symbol nil]
     :else
       [(.substring symbol 0 ns-pos)
        (.substring symbol (inc ns-pos))
        ns-pos])))

(defn- ns-exists
  "Given an string its-name, returns either an ns if a like named ns
  exists, or nil. If its-name is nil, returns nil."
  [#^String its-name]
  (and its-name
       (find-ns (symbol its-name))))

(defn- completion-list
  "Returns a list of vars or nses (depending on value of of-what:
  either :var or :ns) that are possible completions of sym, given that
  maybe-ns is nil or an ns in which to search of vars if of-what
  is :var, and current-ns is the ns of the context of the completion
  or nil."
  [of-what, #^String sym, & [maybe-ns, current-ns]]
  (let [no-ns? (not maybe-ns)
        maybe-ns (or maybe-ns current-ns)]
    (cond
     (= :ns of-what)
       (filter (partial compound-prefix-match \. sym)
               (map (comp name ns-name)
                    (all-ns)))
     (and maybe-ns
          (= :var of-what))
       (map (if no-ns?
              identity
              (partial str maybe-ns \/))
            (filter (partial compound-prefix-match \- sym)
                    (map (comp name :name meta)
                         (filter var? (vals ((if (= maybe-ns current-ns)
                                               ns-map
                                               ns-publics)
                                             maybe-ns)))))))))

(defn- compound-complete
  "Returns a list of possible completions of sym in cur-ns."
  [#^String sym,
   #^String cur-ns]
  (let [[sym-ns sym-name sym-slash] (symbol-name-parts sym)
        sym-ns (ns-exists sym-ns)
        cur-ns (ns-exists cur-ns)]
    (prn sym-ns cur-ns sym-name)
    (if sym-ns
      (completion-list :var sym-name sym-ns cur-ns)
      (concat
       (completion-list :var sym-name nil cur-ns)
       (map #(str % \/)
            (completion-list :ns sym-name))))))

(defn- largest-common-prefix
  "Returns the largest common prefix of two strings."
  ([#^String a #^String b]
     (apply str (take-while (comp not nil?) (map #(when (= %1 %2) %1) a b))))
  {:tag String})

(defslimefn completions [string package]
  (try
   (let [matches (sort (compound-complete string package))
         longest-comp (if matches
                        (reduce largest-common-prefix matches)
                        string)]
     (list matches longest-comp))
   ;; comment the following sexp if debugging completion.
   (catch Throwable e
     (list nil string))))
