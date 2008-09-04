;;;; swank-clojure.el --- slime settings for clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See swank-clojure.clj for instructions
;;;

(eval-and-compile 
  (defvar swank-clojure-path
    (let ((path (file-truename (or (locate-library "swank-clojure")
                                   load-file-name))))
      (and path (file-name-directory path)))
    "Directory containing the swank-clojure package. This is used
to load the supporting clojure library swank."))

(defgroup swank-clojure nil
  "SLIME/swank support for clojure"
  :prefix "swank-clojure-"
  :group 'applications)

(defcustom swank-clojure-java-path "java"
  "The location of the java executable"
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-jar-path nil
  "The location of the jar file for clojure. For example,
/path/to/clojure.jar "
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-extra-classpaths (list (file-truename "~/.clojure/*")) 
  "The classpath from which clojure will load from (passed into
java as the -cp argument). On default, it includes all jar files
within ~/.clojure/"
  :type 'list
  :group 'swank-clojure)




(defun swank-clojure-init (file encoding)
  (format "%S\n\n%S\n\n%S\n\n"
          `(clojure/require 'swank)
          (when (boundp 'slime-protocol-version)
            `(swank/ignore-protocol-version ,slime-protocol-version))
          `(swank/start-server ,file)))

(defun swank-clojure-find-package ()
  (let ((regexp "(\\(clojure/\\)?ns\\W+:?\\(.*\\>\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

(defun swank-clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function) 'swank-clojure-find-package))

(defun swank-clojure-update-indentation (sym indent)
  (put sym 'clojure-indent-function indent))

(defun swank-clojure-cmd ()
  "Create the command to start clojure based off of current configuration settings"
  (when (not swank-clojure-jar-path)
    (error "Error: You must specify a swank-clojure-jar-path. Please see README of swank-clojure."))
  (list swank-clojure-java-path
        "-cp"
        (mapconcat 'identity
                   (append (list swank-clojure-jar-path
                                 swank-clojure-path)
                           swank-clojure-extra-classpaths)
                   path-separator)
        "clojure.lang.Repl"))

;; Change the repl to be more clojure friendly
(defun swank-clojure-slime-repl-modify-syntax ()
  (when (string-match "\\*slime-repl clojure\\*" (buffer-name))
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

(provide 'swank-clojure)