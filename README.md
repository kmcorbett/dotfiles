# kmcorbett/dotfiles

## Fresh dotfiles installation

Note: All shell commands are prefaced with the prompt "$". All the
commands shown here must be run in the user's home directory. In other
words, we assume here that the user has used "cd" to set their working
directory to the default (per-user) home directory path.

### Environment

    $ export TMPDIR="~/tmp"

### Archive un-fresh files

The "fresh" command may file if you attempt to freshen a file that
already exists and is not a link to the fresh source file.

    $ files1=".signature .bash_profile .bash_profile.local .bashrc .tmux.conf"
    $ files2=".gitconfig .emacs.d/init.el .ccl-init.lisp"
    $ tar cvf dotfiles-archive.tar $files1 $files2
    $ rm $files

### Fresh sources from Github

    $ git clone git@github.com:kmcorbett/dotfiles.git .dotfiles

### Freshen for the first time

    $ bash -c "`curl -sL --insecure get.freshshell.com`"
    $ ln -s ~/.dotfiles/freshrc ~/.freshrc
    $ fresh

## Freshen dotfiles 2nd and future times

### Fresh every time

    $ fresh

### (Re)Fresh from Github

    $ fresh update

## Fresh miscellany

#### Install dependencies

### Fresh Emacs

My Emacs dotfiles expect to run in Emacs 24 or later. Follow
instructions for updating Emacs here: 
[https://launchpad.net/~cassou/+archive/emacs](https://launchpad.net/~cassou/+archive/emacs)

To install various dependencies (used by emacs-init.el)

    $ brew upgrade && brew install gs w3m

#### Install mew email reader

Install mew from sources: download, unzip, and build

    $ mewfiles=<location>
    $ cd <root of sources>

(This is from memory and probably does not work - TBD)

    $ make configure && make install

### Fresh Common Lisp using QuickLisp

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
