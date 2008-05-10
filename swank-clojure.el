;;;; swank-clojure.el --- slime settings for clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See swank-clojure.clj for instructions
;;;

(add-to-list 'slime-lisp-implementations
             '(clojure ("clojure") :init clojure-init)
             t)

(eval-and-compile 
  (defvar clojure-swank-path
    (let ((path (file-truename (or (locate-library "swank-clojure")
                                   load-file-name))))
      (and path (file-name-directory path)))
    "Directory containing the swank-clojure package. This is used
     to load the supporting clojure library swank."))

(defun clojure-init (file encoding)
  (format "%S\n\n%S\n\n"
	  `(load-file ,(format "%s/swank-clojure.clj" clojure-swank-path))
          `(swank/on-thread-do (swank/start-swank ,file))))

(defun clojure-slime ()
  (interactive)
  (let ((slime-find-buffer-package-function 'find-clojure-package))
    (slime 'clojure)))

(defun find-clojure-package ()
  (let ((regexp (concat "^(\\(clojure/\\)?in-ns\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

(defun clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function) 'find-clojure-package)
  (make-local-hook 'slime-indentation-update-hooks)
  (add-hook 'slime-indentation-update-hooks 'clojure-update-indentation))

(defun clojure-update-indentation [sym indent]
  (put sym 'clojure-indent-function indent))

(add-hook 'clojure-mode-hook 'clojure-slime-mode-hook t)

(provide 'swank-clojure)