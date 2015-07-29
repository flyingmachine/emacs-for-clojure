Emacs for the dialets of Lisp
=======
The Emacs version must be 24.0+.

## How to use
1. backup your original .emacs.d directory to other place or rename it;
2. git clone it as ~/.emacs.d
3. done!

### Common Lisp repl
1. run into repl: M-x slime
2. exit repl: (exit)

### Clojure repl
1. run into repl: M-x cider-jack-in, M-x cider-connect
2. exit repl: M-x cider-quit

### Magit for git is buildin
M-x magit-* what's your want

## On Mac OSX
Just checkout the osx branch, on Linux/Windows which had not been tested. 
But on OSX it works great. This branch will check the package managements 
and then descided how to install or load the apropos.

### Features
1. compile and load the config/ eclisp files at startup;
2. customized the awesome theme;


## Thanks
1. It's based on the greate job of flyingmachine to see [emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure.git)
2. [emacs tutorial](http://www.braveclojure.com/basic-emacs/)
