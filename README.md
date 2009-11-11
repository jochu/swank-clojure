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

If swank-clojure-classpath is not set within Emacs, it will assume
that you want swank-clojure to handle it for you and will download and
configure the necessary jars itself. If you already have a checkout of
Clojure and/or Contrib that you would like to use, just set
swank-clojure-classpath to a list that includes both those jars as
well as swank-clojure.jar. If you already have a project with all its
dependencies set up, see M-x swank-clojure-project documented below.

(See also Installing from Source below if you want to use the
absolute latest version of swank-clojure.)

## Project

You can also start a Slime session for a given project:

    M-x swank-clojure-project

This will prompt for a project dir and set up the classpath for that
structure based on some existing Clojure conventions:

* src/, classes/, and test/ - added to the classpath
* lib/ - all .jars in here are added to the classpath
* src/main/clojure, src/test/, target/classes, 
  target/dependency - added if pom.xml exists (maven-style)
  All jars in target/dependency will be added as well.

Your project should include *all* its dependent jars (including
Clojure and Swank-Clojure) in either lib/ or target/dependency. If it
depends on more than just Clojure, Contrib, and Swank, it's
recommended that you use a dependency manager such as maven to place
these.

If you add jars to lib/ and want to use them, simply invoke M-x
swank-clojure-project again to restart with them on the classpath.

## Embedding

You can embed swank in your project, start the server from within your
own code, and connect via Emacs to that instance:

    (ns my-app
      (:use [swank.swank :as swank]))
    (swank/start-repl) ;; optionally takes a port argument

Then use M-x slime-connect to connect from within Emacs.

You can also start the server directly from the "java" command-line
launcher if you use "swank.swank" as your main class.

## Usage

Common commands:

* **M-TAB**: Autocomplete symbol at point
* **C-x C-e**: Eval the form under the point
* **C-c C-k**: Compile the current buffer
* **M-.**: Jump to the definition of a var
* **C-c S-i**: Inspect a value
* **C-c C-m**: Macroexpand the call under the point
* **C-c C-d C-d**: Look up documentation for a var
* **C-c C-z**: Switch to the repl buffer

## Keeping Common Lisp

If you want to use SLIME with Common Lisp or another Lisp
implementation, add this to your Emacs config:

    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

Then launch Slime with M-- M-x slime $LISP instead of just M-x slime.

## Community

The [mailing list](http://groups.google.com/group/swank-clojure) 
and #clojure on Freenode are the best places to bring up questions or
issues. Contributions are preferred as either Github pull requests or
using "git format-patch".

Please use standard Emacs indentation with no tabs.

## Installing from Source

Swank-clojure is really two pieces: a server written in Clojure and a
launcher written in Elisp. Using the latest version of the Elisp
should pull in the latest version of the Clojure code as well, but
you'll need to manually install the elisp dependencies as well.

    $ git clone git://github.com/technomancy/slime.git
    $ git clone git://github.com/technomancy/clojure-mode.el

Open slime/slime.el, slime/contrib/slime-repl.el,
clojure-mode/clojure-mode.el, and src/emacs/swank-clojure.el and hit
M-x package-install-from-buffer in each buffer in order. You will get
compiler warnings, but they should not be fatal. Restart Emacs, and
you should be able to use M-x slime.

## License

Copyright (C) 2008-2009 Jeffrey Chu, Phil Hagelberg

This file is licensed under the terms of the GNU General Public
License as distributed with Emacs (press C-h C-c to view it).

[1] - [ELPA](http://tromey.com/elpa/install.html) is the Emacs Lisp
  Package Archive. It brings a real package manager to Emacs.
