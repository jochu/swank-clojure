(ns swank.commands.contrib.swank-arglists
  (:use (swank util core commands)))

(defslimefn completions [string package]
  ((slime-fn 'simple-completions) string package))