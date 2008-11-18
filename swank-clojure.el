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

(defcustom swank-clojure-extra-classpaths (when (file-directory-p "~/.clojure") (directory-files "~/.clojure" t ".jar$"))
  "The classpath from which clojure will load from (passed into
java as the -cp argument). On default, it includes all jar files
within ~/.clojure/"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-library-paths nil
  "The library paths used when loading shared libraries,
used to set the java.library.path property"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-extra-vm-args nil
  "Extra arguments to be passed to the Java VM when starting clojure.
For example -Xmx512m or -Dsun.java2d.noddraw=true"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-binary nil
  "Used as a binary executable (instead of
swank-clojure-java-path) if non-nil."
  :type 'string
  :group 'swank-clojure)


(defun swank-clojure-init (file encoding)
  (format "%S\n\n%S\n\n%S\n\n%S\n\n"
          `(add-classpath ,(concat "file:///" swank-clojure-path))
          `(require 'swank)
          (when (boundp 'slime-protocol-version)
            `(swank/ignore-protocol-version ,slime-protocol-version))
          `(swank/start-server ,file :encoding ,(format "%s" encoding))))

(defun swank-clojure-find-package ()
  (let ((regexp "^(\\(clojure.core/\\)?\\(in-\\)?ns\\s-+[:']?\\(.*\\>\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 3)))))

(defun swank-clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function) 'swank-clojure-find-package))

(defun swank-clojure-update-indentation (sym indent)
  (put sym 'clojure-indent-function indent))

(defun swank-clojure-concat-paths (paths)
  "Concatenate given list of `paths' using `path-separator'. (`expand-file-name'
will be used over paths too.)"
  (mapconcat 'identity (mapcar 'expand-file-name paths) path-separator))

(defun swank-clojure-cmd ()
  "Create the command to start clojure according to current settings."
  (if (and (not swank-clojure-binary) (not swank-clojure-jar-path))
      (error "You must specifiy either a `swank-clojure-binary' or a `swank-clojure-jar-path'")
    (if swank-clojure-binary
	(if (listp swank-clojure-binary)
	    swank-clojure-binary
	  (list swank-clojure-binary))
      (delete-if
       'null
       (append
	(list swank-clojure-java-path)
	swank-clojure-extra-vm-args
	(list
	 (when swank-clojure-library-paths
	   (concat "-Djava.library.path="
		   (swank-clojure-concat-paths swank-clojure-library-paths)))
	 "-classpath"
	 (swank-clojure-concat-paths
	  (cons swank-clojure-jar-path swank-clojure-extra-classpaths))
	 "clojure.lang.Repl"))))))

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
    (modify-syntax-entry ?= "'")

    ;; set indentation function (already local)
    (setq lisp-indent-function 'clojure-indent-function)

    ;; set paredit keys
    (when (featurep 'clojure-paredit)
      (define-key slime-repl-mode-map "{" 'paredit-open-brace)
      (define-key slime-repl-mode-map "}" 'paredit-close-brace))))

(provide 'swank-clojure)
