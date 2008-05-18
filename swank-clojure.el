;;;; swank-clojure.el --- slime settings for clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See swank-clojure.clj for instructions
;;;

(eval-after-load "slime"
  '(require 'clojure-mode))

(eval-and-compile 
  (defvar clojure-swank-path
    (let ((path (file-truename (or (locate-library "swank-clojure")
                                   load-file-name))))
      (and path (file-name-directory path)))
    "Directory containing the swank-clojure package. This is used
     to load the supporting clojure library swank."))

(add-to-list 'slime-lisp-implementations
             '(clojure ("clojure") :init clojure-init)
             t)

(defun clojure-init (file encoding)
  (format "%S\n\n%S\n\n%S\n\n"
          `(load-file ,(file-truename (format "%s/swank-clojure.clj" clojure-swank-path)))
          `(swank/ignore-protocol-version ,slime-protocol-version)
          `(swank/start-server ,file)))

(defun find-clojure-package ()
  (let ((regexp (concat "^(\\(clojure/\\)?in-ns\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

(defun clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function) 'find-clojure-package))

(defun clojure-update-indentation (sym indent)
  (put sym 'clojure-indent-function indent))

;; Change the repl to be more clojure friendly
(defun clojure-slime-repl-modify-syntax ()
  (when (string-match-p "\\*slime-repl clojure\\*" (buffer-name))
    ;; modify syntax
    (modify-syntax-entry ?~ "'   ")
    (modify-syntax-entry ?, "    ")
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (modify-syntax-entry ?^ "'")

    ;; set indentation function (already local)
    (setq lisp-indent-function 'clojure-indent-function)
    
    ;; set paredit keys
    (when (featurep 'clojure-paredit)
      (define-key slime-repl-mode-map "{" 'paredit-open-brace)
      (define-key slime-repl-mode-map "}" 'paredit-close-brace))))

(add-hook 'slime-indentation-update-hooks 'clojure-update-indentation)
(add-hook 'slime-repl-mode-hook 'clojure-slime-repl-modify-syntax t)
(add-hook 'clojure-mode-hook 'clojure-slime-mode-hook t)

(provide 'swank-clojure)