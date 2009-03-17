(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)
        (swank.util string clojure)))

(defn- compound-prefix-match?
  "Takes a `prefix' and a `target' string and returns whether `prefix'
   is a compound-prefix of `target'.

   Viewing each of `prefix' and `target' as a series of substrings
   delimited by `delimiter', if each substring of `prefix' is a prefix
   of the corresponding substring in `target' then we call `prefix' a
   compound-prefix of `target'."
  ([delimiter #^String prefix #^String target]
     (let [prefixes (.split prefix delimiter -1)
           targets (.split target delimiter -1)]
       (when (<= (count prefixes) (count targets))
         (every? true? (map #(.startsWith #^String %1 %2) targets prefixes))))))

(defn- unacronym
  "Interposes delimiter between each character of string."
  ([delimiter #^String string]
     (apply str (interpose delimiter string)))
  {:tag String})

(defn- compound-prefix-match-acronyms?
  ([delimiter prefix target]
     (or (compound-prefix-match? delimiter prefix target)
         (compound-prefix-match? delimiter (unacronym delimiter prefix) target))))

(defn- find-ns-str
  "Given an string its-name, returns either an ns if a like named ns
  exists, or nil. If its-name is nil, returns the default ns."
  ([its-name] (find-ns-str its-name nil))
  ([its-name cur-ns-name]
     (if (nil? its-name)
       (maybe-ns nil)
       (and its-name
            (resolve-ns (symbol its-name) (maybe-ns cur-ns-name))))))

(defn- completion-list-ns
  "Returns a list of nses that are possible compound completions of sym.
   The compound completion delimiter is `.'"
  ([sym cur-ns-name]
     (filter (partial compound-prefix-match-acronyms? "\\." sym)
             (concat (map (comp name ns-name) (all-ns))
                     (map name (keys (ns-aliases (find-ns-str cur-ns-name))))))))

(defn- completion-list-var
  "Returns a list of vars that are possible compound completions of sym,
   given that maybe-ns is nil or an ns in which to search of vars, and
   current-ns is the ns of the context of the completion or nil.
   The compound completion delimiter is `-'"
  ([sym-name sym-ns-name cur-ns-name]
     (let [sym-ns (find-ns-str sym-ns-name cur-ns-name)
           cur-ns (find-ns-str cur-ns-name)
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
                         (partial compound-prefix-match-acronyms? "-" sym-name)
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
            (completion-list-ns sym-name cur-ns-name))))))

(defslimefn completions [string package]
  (let [matches (sort (compound-complete string package))
        longest-comp (if (seq matches)
                       (let [last-char (fn [#^String string]
                                         ;; only for non-empty strings
                                         (.charAt string
                                                  (dec (.length string))))
                             #^String
                             prefix (reduce largest-common-prefix matches)]
                         ;; Remove trailing \- or \. (if completing a
                         ;; namespace) from longest completable
                         ;; string, unless the text before it is not
                         ;; expandable, so that cursor is positioned
                         ;; before the dash or dot.
                         (if (and
                              (not (empty? prefix))
                              (or (= \- (last-char prefix))
                                  (and (not (char-position \/ prefix))
                                       (= \. (last-char prefix))))
                              (not-every? #(.startsWith #^String % prefix)
                                          matches))
                           (.substring prefix 0 (dec (.length prefix)))
                           prefix))
                       string)]
    (list matches longest-comp)))