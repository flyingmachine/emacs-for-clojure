Emacs is a Part of You
=======

I'm a nerd with Emacs, it's awesome if I can [Learn Emacs in less then Ten Years](http://edward.oconnor.cx/2009/07/learn-emacs-in-ten-years). So, I decided to start from Level One not the Level Zero after read Emacs' tutorial (C-h t). Emacs is the most powerful editor on the world there is no **one of**, it's programmable, elegant and **self-documenting**, so you can start from Level Zero if you had time may be less than 10 years to read and try Emacs.

After falling in love with [Clojure](http://clojure.org/), I know it's the time to make Emacs being a part of my body and mind, not just open a file, navigating, editing, and save it. The Level One of mine start from [this is a Clojure-friendly emacs config](https://github.com/flyingmachine/emacs-for-clojure), and [A reasonalbe Emacs config](https://github.com/purcell/emacs.d). But, those ones neither can suit for my daily use on different machines.

The adaptive Emacs which I need is more stable, more smaller, more faster and more flexible which can be run on anywhere where Emacs can run. So, I decide to build a new one.


* [Features](#features)
* [Requirements](#requirements)
* [Install](#install)
* [Where](#where)
* [What](#what)
* [How](#how)

Now, let's start from the Level Two. And one more thing: teach youself some little Elisp (M-x info).

## Features
* __Carefully designed package manager__: just load what's your need, so it is very stable, and fast, the loading time less than 1s on most machines.
* __Adaptive__: Can run on any OS, GUI or Terminal, from most recent Emacs's versions to ancient ones.
* __Consistent__: Whereever you can play with Emacs always behaves in the same way.
* __Awesome style__: See it yourself.

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
* Lisp, ... SBCL, ECL, Clojure, ClojureScript, Racket, Lfe, Scheme, ...
* C/C++ programming/debugging (gdb/lldb)
* Shell, Makefile, Lua, Python, ...
* Erlang, ...
* Bash on Windows, ...
* Emacs can do ...

## How

### Emacs's Documents
It's **self-documenting** and great, keep reading it frequently.

* Tutorial: ```C-h-t```
* Help for Help: ```C-h C-h```
* Emacs manual: ```C-h r```
* Info: ```C-h i```
* Info of mode: ```C-h i d m <x>``` *<x>* is the mode
* Mode: ```C-h-m``` see all the key bindings and documentation of current buffer
* Where is command: ```C-h w``` which keystrokes binding to a given command
* Function: ```C-h-f``` display documentation of the given function
* Variable: ```C-h-v``` display documentation of the given variable
* Keybinding: ```C-h-k``` display documentation of the function invoked by the given keystrokes
* Prefix keybindings: ```<prefix> C-h``` see all key bindings for given prefix keystrokes
* Keybinding briefly: ```C-h c```, which command for given keystroke
* Message: ```C-h e``` see the logging of echo area message
* Man: ```M-x man``` view UNIX manual page
* Woman: ```M-x woman``` view UNIX manual page without ```man``` program

### Motion
* goto line: ```M-g g```
* goto nth char: ```M-g c```
* pop global mark: ```C-x C-@```

### Interaction
* \*scratch\* buffer
* via Elisp: ```M-:```
* via Shell Command: ```M-!```, see *Info>Emacs>Shell*
* in Dired mode: ```!```, do shell command
* insert shell output: ```C-u M-!```

### Editing
* kill whole line: ```C-S-DEL```
* kill all spaces at point: ```M-\```
* kill all spaces except one at point: ```M-SPC```
* dynamic abbreviation: ```M-/```
* query replace: ```M-%```
* what cursor position: ```C-x =```

### Basic sexp commands
* ```forward-sexp```: ```C-M-f```
* ```backward-sexp```: ```C-M-b```
* ```kill-sexp```: ```C-M-k```
* ```transpose-sexp```: ```C-M-t```

### Window
* dired other window: ```C-x 4 d```
* find file in other window: ```C-x 4 C-f```, or ```C-x 4 f```
* display buffer: ```C-x 4 C-o```, display the buffer in another window
* find tag other window: ```C-x 4 .```
* kill buffer and window: ```C-x 4 0```, just like ```C-x 0``` except kill the buffer
* switch to buffer other window: ```C-x 4 b```
* clone indirect buffer other window: ```C-x 4 c```, clone the buffer in another window

### Frame
* find file other frame: ```C-x 5 C-f```, or ```C-x 5 f```
* display buffer other frame: ```C-x 5 C-o```, or ```C-x 5 b```
* find tag other frame: ```C-x 5 .```
* delete frame: ```C-x 5 0```
* delete other frames: ```C-x 5 1```
* make frame command: ```C-x 5 2```
* dired to other frame: ```C-x 5 d```
* other frame: ```C-x 5 o```
* find file read only other frame: ```C-x 5 r```


### Register
* window configuration to register: ```C-x r w <REG>```, <REG> is single character, it can not accross sessions.
* frame configuration to register: ```C-x r f <REG>```
* point to register: ```C-x r <SPC> <REG>```
* jump to register: ```C-x r j <REG>```
* copy to register: ```C-x r s <REG>```
* insert register: ```C-x r i <REG>```
* view register

### Region


### Keyboard Macro
* start recording macro: ```C-x (```
* stop recording macro: ```C-x )```
* playback macro: ```C-x e```
* apply macro to region lines: ```C-x C-k r```


### Shell
* EShell: ```M-x eshell```
* Shell: ```M-x shell```
* Ansi-Term: ```M-x ansi-term```


### Remote
It's the duty of [TRAMP](https://www.gnu.org/software/tramp/)
* non-sudo: ```C-x C-f /ssh:<remote-id>:/path/to/file RET```, *<remote-id>* such as *x@y.z* or *xyz* in .ssh/config entries.
* sudo: ```C-x C-f /ssh:<remote-id>|sudo:<remote-host>:/path/to/file```, *<remote-host>*
such as *x@localhost* or *localhost* if the user *x* is a sudoer.
* eshell remote: ```cd /ssh:<user>@<remote>:<dir>```

### Cook
* copy __private/self-sample.el__ to __private/self.el__
* cooking

