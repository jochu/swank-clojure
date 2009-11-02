# Swank Clojure

[Swank-clojure](http://github.com/technomancy/swank-clojure) is an
adapter that lets you use SLIME (the Superior Lisp Interaction Mode
for Emacs) with Clojure.

## Installation

1. Install from ELPA using package.el[1].
        
2. Press M-x slime to start a new Clojure process. Press "y" when
   asked if you want to install Clojure.

3. Do you seriously need a third step? OK, if you want to use a newer
   version of Clojure than 1.0 you will need to build it yourself and
   symlink the compiled jar to ~/.swank-clojure/clojure-$VERSION.jar
   after removing the old version.

## Project

You can also start a Slime session for a given project:

    M-x swank-clojure-project

This will prompt for a project dir and set up the classpath for that
structure based on some existing Clojure conventions:

* src/, classes/, and test/ - added to the classpath
* lib/ - all .jars in here are added to the classpath
* src/main/clojure, src/test/, target/classes, 
  target/dependency - added if pom.xml exists (maven-style)

You can embed swank in your project and connect via Emacs to an
already-running instance. TODO: explain this.

## Usage

Common commands:

* M-TAB: Autocomplete symbol at point
* C-x C-e: Eval the form under the point
* C-c C-k: Compile the current buffer
* M-.: Jump to the definition of a var
* C-c S-i: Inspect a value
* C-c C-m: Macroexpand the call under the point
* C-c C-d C-d: Look up documentation for a var
* C-c C-z: Switch to the repl buffer

## Keeping Common Lisp

If you want to use SLIME with Common Lisp or another Lisp
implementation, add this to your Emacs config:

    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

Then launch Slime with M-- M-x slime $LISP instead of just M-x slime.

## Community

The mailing list is at http://groups.google.com/group/swank-clojure
and the author can often be contacted in #clojure on
Freenode. Contributions are preferred as either Github pull requests
or using "git format-patch".

## License

Copyright (C) 2008-2009 Jeffrey Chu, Phil Hagelberg

This file is licensed under the terms of the GNU General Public
License as distributed with Emacs (press C-h C-c to view it).

[1] - [ELPA](http://tromey.com/elpa/install.html) is the Emacs Lisp
  Package Archive. If you need to install manually for some reason,
  use M-x package-install-from-buffer. You will need to handle the
  dependencies manually in that case: slime.el and clojure-mode.el.

