(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)))

(defslimefn completions [string package]
  ((slime-fn 'simple-completions) string package))