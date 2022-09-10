# this is a Clojure-friendly emacs config

If you're new to emacs, check out
[this introductory tutorial](http://www.braveclojure.com/basic-emacs/)!

## Installing

1. Close Emacs.
2. Delete `~/.emacs` or `~/.emacs.d` if they exist. (Windows users, your
   emacs files will probably live in
   `C:\Users\your_user_name\AppData\Roaming\`. So, for example, you
   would delete `C:\Users\jason\AppData\Roaming\.emacs.d`.) This is
   where Emacs looks for configuration files, and deleting these files
   and directories will ensure that you start with a clean slate.
3. Download the Emacs
   [configuration zip file](https://github.com/flyingmachine/emacs-for-clojure/archive/book1.zip)
   and unzip it. Its contents should be a folder,
   `emacs-for-clojure-book1`. Run `mv path/to/emacs-for-clojure-book1
   ~/.emacs.d`.

Then open Emacs. The first time you start, it will take a few minutes,
because it needs to download and install around fifty packages. You
will see some warnings pop up, but they are only style suggestions for
the packages being loaded.

## Features
This will allow you to edit Clojure files with syntax-aware
highlighting and "structural" editing via paredit, which means it will
keep all your delimiters for nested forms balanced (think parens, square
brackets, and curly braces). Check out [this animated
guide](http://danmidwood.com/content/2014/11/21/animated-paredit.html)
to paredit. It's one of those things that seems strange at first, but
once you get used to it, you won't want to edit Clojure without it!

Other excellent capabilities you'll want to know about include:

* [CIDER](https://cider.mx/), a fully interactive Clojure environent
* [clojure-lsp](https://clojure-lsp.io/), provides static analysis
  features for Clojure, such as live style and syntax warnings
* [Projectile](https://projectile.mx/), navigate and manage project
  files
* [Magit](https://magit.vc/), a complete interface to git
* [Treemacs](https://github.com/Alexander-Miller/treemacs), a tree
  layout file explorer

## Upgrading
Each package we use gets updated by its authors, at whatever cadence works for
them. It's a good idea to stay up-to-date, to get improvements and bug
fixes. It's analogous to keeping the software up-to-date in your operating
system.

When you run `M-x list-packages` it refreshes the cache of all the package
repositories, and then tells you in the status line whether there are any
updates to install. Press `U` to mark all upgradeable packages for installation,
and then press `x` to execute the installation. You will be prompted to confirm,
and when you press `y` the package updates will be installed. Press `q` to exit
the package list when it's finished.

If you ever get curious to look, you can find all the installed packages in `~/.emacs.d/elpa`.

## Organization

I've tried to separate everything logically and document the purpose
of every line. [`init.el`](./init.el) acts as a kind of table of
contents.  It's a good idea to eventually go through `init.el` and the
files under the `customizations` directory so that you know exactly
what's going on.

## Supporting CSS, HTML, JS, etc.

Emacs has decent support for CSS, HTML, JS, and many other file types
out of the box, but if you want better support, then have a look at 
[my personal emacs config's
init.el](https://github.com/flyingmachine/emacs.d/blob/master/init.el). 
It's meant to read as a table of contents. The emacs.d as a whole adds the following:

* [Customizes js-mode and html editing](https://github.com/flyingmachine/emacs.d/blob/master/customizations/setup-js.el)
    * Sets indentation level to 2 spaces for JS
    * enables subword-mode so that M-f and M-b break on capitalization changes
    * Uses `tagedit` to give you paredit-like functionality when editing html
    * adds support for coffee mode
* [Uses enh-ruby-mode for ruby
editing](https://github.com/flyingmachine/emacs.d/blob/master/customizations/setup-ruby.el). 
enh-ruby-mode is a little nicer than the built-in ruby-mode, in my opinion.
    * Associates many filenames and extensions with enh-ruby-mode (.rb, .rake, Rakefile, etc)
    * Adds keybindings for running specs
* Adds support for YAML and SCSS using the yaml-mode and scss-mode packages

In general, if you want to add support for a language then you should
be able to find good instructions for it through Google. Most of the
time, you'll just need to install the "x-lang-mode" package for it.
