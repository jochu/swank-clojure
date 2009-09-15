(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)
        (swank.commands completion)
        (swank.util string clojure)))

(defn- compound-prefix-match?
  "Takes a `prefix' and a `target' string and returns whether `prefix'
   is a compound-prefix of `target'.

   Viewing each of `prefix' and `target' as a series of substrings
   delimited by `delimiter', if each substring of `prefix' is a prefix
   of the corresponding substring in `target' then we call `prefix' a
   compound-prefix of `target'."
  ([delimiter #^String prefix #^String target]
     (let [prefixes (.split prefix (str "[" delimiter "]") -1)
           targets  (.split target (str "[" delimiter "]") -1)]
       (when (<= (count prefixes) (count targets))
         (every? true? (map #(.startsWith #^String %1 %2) targets prefixes))))))

(defn- unacronym
  "Interposes delimiter between each character of string."
  ([delimiter #^String string]
     (apply str (interpose (first delimiter) string)))
  {:tag String})

(defn- compound-prefix-match-acronyms?
  "Checks if `prefix' is a compound-prefix of `target'. If it is
   not, try it as an acronym."
  ([delimiter prefix target]
     (or (compound-prefix-match? delimiter prefix target)
         (compound-prefix-match? delimiter (unacronym delimiter prefix) target))))

(defslimefn completions [symbol-string package]
  (try
   (let [[sym-ns sym-name] (symbol-name-parts symbol-string)
         potential         (potential-completions (when sym-ns (symbol sym-ns)) (ns-name (maybe-ns package)))
         matches           (seq (sort (filter #(compound-prefix-match-acronyms? "-/\\." symbol-string %) potential)))]
     (list matches
           (if matches
             (reduce largest-common-prefix matches)
             symbol-string)))
   (catch java.lang.Throwable t
     (list nil symbol-string))))
