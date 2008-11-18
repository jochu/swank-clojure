(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)
        (swank.util string clojure)))

(defn- compound-prefix-match
  "Takes a `prefix' and a `target' string and which returns position
  of last character in `target' which matches `prefix' if `prefix' is
  a compound-prefix of `target', and otherwise nil.

  Viewing each of `prefix' and `target' as a series of substrings
  delimited by `delimeter', if each substring of `prefix' is a prefix
  of the corresponding substring in `target' then we call `prefix' a
  compound-prefix of `target'."
  ([delimeter #^String prefix #^String target]
     (if (empty? prefix)
       0
       (loop [prefix prefix, tpos 0]
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
               (recur newprefix (inc new-tpos))
               new-tpos)))))))

(defn- unacronym
  "Interposes delimiter between each character of string."
  ([delimiter #^String string]
     (apply str (interpose delimiter string)))
  {:tag String})

(defn- compound-prefix-match-acronyms
  ([delimiter prefix target]
     (or (compound-prefix-match delimiter prefix target)
         (compound-prefix-match delimiter (unacronym delimiter prefix) target))))

(defn- string-position
  "Like position, but specifically for Strings and using Java
  methods."
  [chr,
   #^String string]
  (let [idx (.indexOf string (int chr))]
    (when (not= -1 idx)
      idx)))

(defn- ns-find-string
  "Given an string its-name, returns either an ns if a like named ns
  exists, or nil. If its-name is nil, returns nil."
  [its-name]
  (and its-name
       (find-ns (symbol its-name))))

(defn- completion-list-ns
  "Returns a list of nses that are possible compound completions of sym.
   The compound completion delimeter is `.'"
  ([sym]
     (filter (partial compound-prefix-match-acronyms \. sym)
             (map (comp name ns-name) (all-ns)))))

(defn- completion-list-var
  "Returns a list of vars that are possible compound completions of sym,
   given that maybe-ns is nil or an ns in which to search of vars, and
   current-ns is the ns of the context of the completion or nil.
   The compound completion delimeter is `-'"
  ([sym-name sym-ns-name cur-ns-name]
     (let [sym-ns (ns-find-string sym-ns-name)
           cur-ns (ns-find-string cur-ns-name)
           vars-of-ns (delay
                       (filter
                        var?
                        (vals (if (or (not sym-ns-name)
                                      (= sym-ns cur-ns))
                                ;; In current namespace complete to all
                                ;; vars, in other namespaces -- only to
                                ;; public vars.
                                (ns-map cur-ns)
                                (ns-publics sym-ns)))))
           completions (delay
                        (filter
                         (partial compound-prefix-match-acronyms \- sym-name)
                         (map
                          (comp name :name meta) ;name of var as String
                          (force vars-of-ns))))]
       (cond
        (and sym-ns-name (not sym-ns))  ;invalid ns given
        nil
        (and sym-ns-name sym-ns)        ;valid ns given
        (map (partial str sym-ns-name "/")
             (force completions))
        :else                           ;no ns given
        (force completions)))))

(defn- compound-complete
  "Returns a list of possible completions of sym in cur-ns."
  [sym cur-ns-name]
  (let [[sym-ns-name sym-name] (symbol-name-parts sym)]
    (if sym-ns-name
      (completion-list-var sym-name sym-ns-name cur-ns-name)
      (concat
       (completion-list-var sym-name nil cur-ns-name)
       (map #(str % \/)
            (completion-list-ns sym-name))))))

(defslimefn completions [string package]
  (let [matches (sort (compound-complete string package))
        longest-comp (if matches
                       (let [last-char (fn [#^String string]
                                         ;; only for non-empty strings
                                         (.charAt string
                                                  (dec (.length string))))
                             prefix (reduce largest-common-prefix matches)]
                         ;; Remove trailing \- or \. (if completing a
                         ;; namespace) from longest completable
                         ;; string, unless the text before it is not
                         ;; expandable, so that cursor is positioned
                         ;; before the dash or dot.
                         (if (and
                              (not (empty? prefix))
                              (or (= \- (last-char prefix))
                                  (and (not (string-position \/ prefix))
                                       (= \. (last-char prefix))))
                              (not-every? #(.startsWith % prefix)
                                          matches))
                           (.substring prefix 0 (dec (.length prefix)))
                           prefix))
                       string)]
    (list matches longest-comp)))