(ns swank.commands.indent
  (:use (swank util core)
        (swank.core hooks connection)
        (swank.util hooks)))

;; TODO - make this readable
(defn- need-full-indentation-update?
  "Return true if the whole indentation cache should be updated. This
   is a heuristic to avoid scanning all symbols all the time: instead,
   we only do a full scan if the set of packages as changed."
  ([conn]
     (not= (hash (all-ns)) (hash @(conn :indent-cache-pkg)))))

(defn- var-indentation [var]
  (flet [(fn indent-loc [meta]
           (or (meta :indent)
               (when-let [arglists (:arglists meta)]
                 (let [arglist (apply min-key #(or (position '& %1) 0) arglists)
                       amp (position '& arglist)
                       body (position 'body arglist)]
                   (when (and body amp
                              (= (- body amp) 1))
                     amp)))))
         (fn indent-cons [meta]
           (when-let [indent-to (indent-loc meta)]
             (when (or (= indent-to 'defun) (>= indent-to 0))
               `(~(str (:name meta)) . ~indent-to))))]
    (indent-cons (meta var))))

(defn- var-indentations-for [nss]
  (filter identity
          (map (comp var-indentation val)
               (filter (comp var? val) (mapcat ns-map nss)))))

(defn- every-other [coll]
  (when coll
    (lazy-cons
     (first coll)
     (every-other (drop 2 coll)))))

(defn- update-indentation-delta
  "Update the cache and return the changes in a (symbol '. indent) list.
   If FORCE is true then check all symbols, otherwise only check
   symbols belonging to the buffer package"
  ([cache force]
     (let [cache-val @cache]
       (flet [(fn in-cache? [[sym var]]
                (let [indent (var-indentation var)]
                  (when indent
                    (when-not (= (cache-val sym) indent)
                      (list sym indent)))))
              (fn considerations-for [nss]
                (let [vars (filter (comp var? val) (mapcat ns-map nss))]
                  (mapcat in-cache? vars)))]
         (if force
           (when-let [updates (considerations-for (all-ns))]
             (dosync (apply alter cache assoc updates))
             (every-other (rest updates)))
           (let [ns (maybe-ns *current-package*)
                 in-ns? (fn [[sym var]] (and (var? var) (= ns ((meta var) :ns))))]
             (when ns
               (when-let [updates (filter identity (considerations-for (list ns)))]
                 (dosync (apply alter cache assoc updates))
                 (every-other (rest updates))))))))))

(defn- perform-indentation-update
  "Update the indentation cache in connection and update emacs.
   If force is true, then start again without considering the old cache."
  ([conn force]
     (let [cache (conn :indent-cache)]
       (let [delta (update-indentation-delta cache force)]
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
