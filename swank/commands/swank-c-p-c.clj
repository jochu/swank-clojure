(in-ns 'swank.commands.contrib)

(defslimefn completions [string package]
  ((slime-fn 'simple-completions) string package))