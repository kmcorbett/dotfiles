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

### Fresh Quicklisp

    $ cd
    $ curl -O http://beta.quicklisp.org/quicklisp.lisp
    $ ccl
    ? (load "~/quicklisp.lisp")
    ? (quicklisp-quickstart:install)
    ? (ql:quickload "quicklisp-slime-helper")

### Fresh every time

    $ fresh

### (Re)Fresh from Github

    $ fresh update
    
### Freshen file modes

    $ chmod +x ~/bin/tunnel2code.sh
