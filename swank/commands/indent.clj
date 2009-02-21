(ns swank.commands.indent
  (:use (swank util core)
        (swank.core hooks connection)
        (swank.util hooks)))

(defn- need-full-indentation-update?
  "Return true if the indentation cache should be updated for all
   namespaces. 

   This is a heuristic so as to avoid scanning all symbols from all
   namespaces. Instead, we only check whether the set of namespaces in
   the cache match the set of currently defined namespaces."
  ([connection]
     (not= (hash (all-ns))
           (hash @(connection :indent-cache-pkg)))))

(defn- find-args-body-position
  "Given an arglist, return the number of arguments before 
     [... & body]
   If no & body is found, nil will be returned"
  ([args]
     (when-let [amp-position (position '& args)]
       (when-let [body-position (position 'body args)]
         (when (= (inc amp-position) body-position)
           amp-position)))))

(defn- find-arglists-body-position
  "Find the smallest body position from an arglist"
  ([arglists]
     (let [positions (remove nil? (map find-args-body-position arglists))]
       (when-not (empty? positions)
         (apply min positions)))))

(defn- find-var-body-position
  "Returns a var's :indent override or the smallest body position of a
   var's arglists"
  ([var]
     (let [var-meta (meta var)]
       (or (:indent var-meta)
           (find-arglists-body-position (:arglists var-meta))))))

(defn- var-indent-representation
  "Returns the slime indentation representation (name . position) for
   a given var. If there is no indentation representation, nil is
   returned."
  ([var]
     (when-let [body-position (find-var-body-position var)]
       (when (or (= body-position 'defun)
                 (not (neg? body-position)))
         (list (name (:name (meta var)))
               '.
               body-position)))))

(defn- var-indentations-in-ns
  "Find the var indentations representation for all the given
   namespaces"
  ([ns & more-ns]
     (for [var (mapcat (comp vals ns-interns) (cons ns more-ns))
           :when (var-indent-representation var)]
       (var-indent-representation var))))

;; TODO - make this readable
(defn- every-other [coll]
  (lazy-seq
   (when (seq coll)
     (cons (first coll)
           (every-other (drop 2 coll))))))

(defn- update-indentation-delta
  "Update the cache and return the changes in a (symbol '. indent) list.
   If FORCE is true then check all symbols, otherwise only check
   symbols belonging to the buffer package"
  ([cache force]
     (let [cache-val @cache]
       (flet [(fn in-cache? [[sym var]]
                (let [indent (var-indent-representation var)]
                  (when (seq indent)
                    (when-not (= (cache-val sym) indent)
                      (list sym indent)))))
              (fn considerations-for [nss]
                (let [vars (filter (comp var? val) (mapcat ns-map nss))]
                  (mapcat in-cache? vars)))]
         (if force
           (when-let [updates (seq (considerations-for (all-ns)))]
             (dosync (apply alter cache assoc updates))
             (every-other (rest updates)))
           (let [ns (maybe-ns *current-package*)
                 in-ns? (fn [[sym var]] (and (var? var) (= ns ((meta var) :ns))))]
             (when ns
               (when-let [updates (seq (filter identity (considerations-for (list ns))))]
                 (dosync (apply alter cache assoc updates))
                 (every-other (rest updates))))))))))

(defn- perform-indentation-update
  "Update the indentation cache in connection and update emacs.
   If force is true, then start again without considering the old cache."
  ([conn force]
     (let [cache (conn :indent-cache)]
       (let [delta (seq (update-indentation-delta cache force))]
         (dosync
          (ref-set (conn :indent-cache-pkg) (hash (all-ns)))
          (when delta
            (send-to-emacs `(:indentation-update ~delta))))))))

(defn- sync-indentation-to-emacs
  "Send any indentation updates to Emacs via emacs-connection"
  ([]
     (perform-indentation-update
      *current-connection*
      (need-full-indentation-update? *current-connection*))))

(add-hook *pre-reply-hook* #'sync-indentation-to-emacs)
