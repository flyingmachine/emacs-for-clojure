Emacs is Part of You
=======

I'm a nerd with Emacs, it's awesome if I can [Learn Emacs in less then Ten Years](http://edward.oconnor.cx/2009/07/learn-emacs-in-ten-years). So, I decided to start from Level One not the Level Zero after read Emacs' tutorial (C-h t). Emacs is the most powerful editor on the world there is no **one of**, it's programmable, elegant and **self-documenting**, so you can start from Level Zero if you had time may be less than 10 years to read and try Emacs.

After falling in love with [Clojure](http://clojure.org/), I know it's the time to make Emacs being a part of my body and mind, not just open a file, navigating, editing, and save it. The Level One of mine start from [this is a Clojure-friendly emacs config](https://github.com/flyingmachine/emacs-for-clojure), and [A reasonalbe Emacs config](https://github.com/purcell/emacs.d). But, those ones neither can suit for my daily use on different machines.

The adaptive Emacs which I need is more stable, more smaller, more faster and more flexible which can be run on anywhere where Emacs can run. So, I decide to build a new one.


* [Features](#features)
* [Requirements](#requirements)
* [Install](#install)
* [Where](#where)
* [What](#what)


Now, let's start from the Level Two. And one more thing: tech youself some little Elisp (M-x info).

## Features
* __Carefully designed package manager__: just load what's your have and what's your need, so it is very stable, and fast, the loading time less than 1s on most machines.
* __Adaptive__: Can run on any OS, GUI or Terminal, recent Emacs's versions or lower versions.
* __Consistent__: Whereever you can play with Emacs always behaves in the same way.
* __Awesome style__

## Requirements
* Emacs installation, the version 24+ is more suitable;
* Any keyboard, not mouse;

## Install
Just one thing you need to do, clone it (just <400K) to your HOME directory:
```sh
$ cd
$ git clone --depth=1 --branch=master https://github.com/junjiemars/.emacs.d.git
```
Then start Emacs and waiting a while for self-install packages, if you had an Emacs 24+

On Windows, if you'd [Git-Bash](https://git-scm.com/downloads) installed but no Emacs, you are lucky, a one line code will do it all for you and more: fix some Emacs' issue for you, 
and you don't need run ```git clone ...```

```sh
$ HAS_EMACS=1 bash <(curl https://raw.githubusercontent.com/junjiemars/kit/master/win/install-win-kits.sh)
```

## Where
* Linux, whatever GUI or Terminal;
* OSX, whatever GUI or Terminal;
* Windows, whatever GUI or Terminal;

## What
* Lisp, ... Clojure, ClojureScript, Racket, lfe, Scheme, ...
* C/C++ programming/debugging (gdb/lldb)
* Shell, Makefile, Lua, Python, ...
* Erlang, ...
* Bash on Windows, ...
* Emacs can do ...

