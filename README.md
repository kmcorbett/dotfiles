# kmcorbett/dotfiles

## Fresh dotfiles installation

### Archive before Fresh

    $ cd
    $ files=".signature .bash_profile .bashrc .emacs.d/init.el .tmux.conf .ccl-init.lisp"
    $ tar cvf dotfiles-archive.tar $files
    $ rm $files

### Fresh from Github

    $ cd
    $ git clone git@github.com:kmcorbett/dotfiles.git .dotfiles

### Fresh for the first time

    $ cd
    $ bash -c "`curl -sL --insecure get.freshshell.com`"
    $ ln -s ~/.dotfiles/freshrc ~/.freshrc
    $ fresh

## Freshen dotfiles 2nd and more times

### Fresh every time

    $ fresh

### (Re)Fresh from Github

    $ fresh update

## Fresh miscellany

### Fresh Emacs

My Emacs dotfiles expect to run in Emacs 24 or later. Follow
instructions for updating Emacs here: 
[https://launchpad.net/~cassou/+archive/emacs](https://launchpad.net/~cassou/+archive/emacs)

### Fresh Quicklisp

    $ cd
    $ curl -O http://beta.quicklisp.org/quicklisp.lisp
    $ ccl
    ? (load "~/quicklisp.lisp")
    ? (quicklisp-quickstart:install)
    ? (ql:quickload "quicklisp-slime-helper")

Next time:

    $ ccl
    ? (load "~/quicklisp/setup.lisp")
    
### Freshen file modes

    $ chmod +x ~/bin/tunnel2code.sh
