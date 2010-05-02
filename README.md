# Swank Clojure

[Swank-clojure](http://github.com/technomancy/swank-clojure) is an
adapter that lets you use SLIME (the Superior Lisp Interaction Mode
for Emacs) with Clojure. It's designed to work with GNU Emacs 23 and
higher. It may work with forks like XEmacs/Aquamacs or earlier
versions of GNU Emacs, but those are not as well supported.

## Usage

There are two main ways to launch Swank Clojure:
        
* Standalone Session: If you just hit M-x slime, swank-clojure will
  download the jars for the latest stable releases of Clojure,
  Contrib, and Swank Clojure, launch an instance, and connect to
  it. If you just want to try out Clojure, this is all you need. Just
  get Swank Clojure through [ELPA](http://tromey.com/elpa) and stop
  reading here. =) But this is intended for experimentation; for real
  work in the context of a project you will probably want the other
  option.

* Project: Users of Leiningen or Maven can [start a session from a
  shell](http://wiki.github.com/technomancy/leiningen/emacs-integration)
  with "lein swank" or "mvn -o clojure:swank" and connect to it from
  within Emacs using M-x slime-connect.

Because the JVM classpath can't be modified at runtime, you can't
start a session with plain M-x slime and then decide to work on your
project; you'll need to start a new session.

## Installation

Install [from ELPA](http://tromey.com/elpa) using package.el[1].

When you perform the installation, you will see warnings related to
the byte-compilation of the packages. This is **normal**; the packages
will work just fine even if there are problems byte-compiling it upon
installation.

If you're going to use Leiningen or Maven to initiate your sessions
(#2 above), you'll only need the "slime-repl" package. Otherwise get
the "swank-clojure" package.

While it's possible to install swank-clojure manually, it's not
recommended. package.el will be included in the next version of Emacs and
has been a standard piece of the Emacs ecosystem for a while
now. See the "Installing from Source" section below if you wish to
hack on a development version that hasn't been released yet.

Be sure you don't have old versions of SLIME either manually installed
or installed using a system-wide package manager like apt-get. If you
do have any manual configuration of SLIME, be sure to either remove it
or place it after package.el is initialized.

## Embedding

You can embed swank in your project, start the server from within your
own code, and connect via Emacs to that instance:

    (ns my-app
      (:use [swank.swank :as swank]))
    (swank/start-repl) ;; optionally takes a port argument

Then use M-x slime-connect to connect from within Emacs.

You can also start the server directly from the "java" command-line
launcher if you AOT-compile it and specify "swank.swank" as your main
class.

## Commands

Commonly-used SLIME commands:

* **M-TAB**: Autocomplete symbol at point
* **C-x C-e**: Eval the form under the point
* **C-c C-k**: Compile the current buffer
* **M-.**: Jump to the definition of a var
* **C-c S-i**: Inspect a value
* **C-c C-m**: Macroexpand the call under the point
* **C-c C-d C-d**: Look up documentation for a var
* **C-c C-z**: Switch from a Clojure buffer to the repl buffer
* **C-c M-p**: Switch the repl namespace to match the current buffer

Pressing "v" on a stack trace a debug buffer will jump to the file and
line referenced by that frame if possible.

Note that Slime was designed to work with Common Lisp, which has a
distinction between interpreted code and compiled code. Clojure has no
such distinction, but many of the Slime commands retain parallel
load/compile commands that have the same effect in the context of
Clojure.

## Debug Repl

For now, see [Hugo Duncan's
blog](http://hugoduncan.org/post/2010/swank_clojure_gets_a_break_with_the_local_environment.xhtml)
for an explanation of this excellent feature. Further documentation to come.

## Alternate Usage

There are a couple other ways to launch a session for special cases:

* Custom classpath: If you want to hack on Clojure or Contrib or want
  to provide your own copy of the jars for some other reason, set
  swank-clojure-classpath to a list of paths to the jars you want to
  use and then hit M-x slime. If you have a ~/.clojure or
  ~/.swank-clojure directory full of jars you want to use, you don't
  need to set swank-clojure-classpath.

* Project Session from Emacs: Put your project's dependencies in the
  lib/ directory, (either manually or using
  [Leiningen](http://github.com/technomancy/leiningen) or Maven) then
  launch M-x swank-clojure-project. Note that you must have
  swank-clojure.jar in the lib/ directory, it will not automatically
  add itself to the classpath as it did in past versions that had to
  run from a checkout.

If you intend to use M-x swank-clojure-project, it will prompt for a
project dir and set up the classpath for that structure based on some
existing Clojure conventions:

* src/, classes/, test/, and resources/ - added to the classpath
* lib/ - all .jars in here are added to the classpath
* src/main/clojure, src/test/, target/classes, 
  target/dependency - added if pom.xml exists (maven-style)
  All jars in target/dependency will be added as well.

Your project should include *all* its dependent jars (including
Clojure and Swank-Clojure) in either lib/ or target/dependency. If it
depends on more than just Clojure, Contrib, and Swank, it's
recommended that you use a dependency manager such as Leiningen to
manage these.

## Keeping Common Lisp

If you want to use SLIME with Common Lisp or another Lisp
implementation, add this to your Emacs config:

    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

Then launch Slime with M-- M-x slime $LISP instead of just M-x slime.

## Community

The [mailing list](http://groups.google.com/group/swank-clojure) and
clojure channel on Freenode are the best places to bring up
questions/issues.

Contributions are preferred as either Github pull requests or using
"git format-patch". Please use standard indentation with no tabs,
trailing whitespace, or lines longer than 80 columns. See [this post
on submitting good patches](http://technomancy.us/135) for some
tips. If you've got some time on your hands, reading this [style
guide](http://mumble.net/~campbell/scheme/style.txt) wouldn't hurt
either.

## Installing from Source

Swank-clojure is really two pieces: a server written in Clojure and a
launcher written in Elisp. The elisp parts are installed with:

    $ git clone git://github.com/technomancy/slime.git
    $ git clone git://github.com/technomancy/clojure-mode.git

Open slime/slime.el, slime/contrib/slime-repl.el,
clojure-mode/clojure-mode.el, and swank-clojure.el and hit
M-x package-install-from-buffer in each buffer in order. You will get
compiler warnings, but they should not be fatal. Restart Emacs, and
you should be able to use M-x slime.

The Clojure-side server is managed with
[Leiningen](http://github.com/technomancy/leiningen). Use the "lein
install" task to place it in your local repository.

Note that using Slime from CVS trunk is not recommended; changes have
been introduced which are incompatible with the current implementation
of the Clojure server. Using the versions in git from above will
ensure that you have a compatible version.

## License

Copyright (C) 2008-2010 Jeffrey Chu, Phil Hagelberg, Hugo Duncan, and contributors

The Clojure server is licensed under the EPL. (See the file COPYING.)
The Emacs code is licensed under the GNU General Public License
version 3, which should have been distributed with your copy of Emacs.

[1] - [ELPA](http://tromey.com/elpa/install.html) is the Emacs Lisp
  Package Archive. It brings a real package manager to Emacs.
