# kmcorbett/dotfiles

## Fresh dotfiles installation

All the commands shown here must be run in the user's home
directory. In other words, we assume here that the user has used "cd"
to set their working directory to the default (per-user) home
directory path.

### Environment

    export TMPDIR="~/tmp"

### Archive un-fresh files

The "fresh" command may complain if you attempt to freshen a file that
already exists and is not a link to the fresh source file.

    tar cvf dotfiles-archive.tar [file...]
    rm $files

### Fresh sources from Github

    git clone git@github.com:kmcorbett/dotfiles.git .dotfiles

### Freshen for the first time

    bash -c "`curl -sL --insecure get.freshshell.com`"
    ln -s ~/.dotfiles/freshrc ~/.freshrc
    fresh

## Freshen dotfiles 2nd and future times

### Fresh every time

    fresh

### (Re)Fresh from Github

To freshen dotfiles with sources on Github:

    fresh update

Note this only works if current branch master is up to date.
In other words, this attempts to pull from remote origin/master iff
there are no local uncommitted changes.

## Miscellaneous apps and tools

### Install dependencies

### Fresh Emacs

My Emacs dotfiles expect to run in Emacs 24 or later. Follow
instructions for updating Emacs here: 
[https://launchpad.net/~cassou/+archive/emacs](https://launchpad.net/~cassou/+archive/emacs)

To install various dependencies (used by emacs-init.el)

    brew upgrade && brew install gs w3m

### Clozure Common Lisp (ccl)

Configure ccl initialization in home/bash_profile.<hostname>

    cd ~/Tools
    svn co http://svn.clozure.com/publicsvn/openmcl/release/1.10/darwinx86/ccl
    cd ccl
    ./scripts/ccl -n
    ? (ccl:rebuild-full :full t)    

### Fresh Common Lisp using QuickLisp

    curl -O http://beta.quicklisp.org/quicklisp.lisp
    ccl -n
    ? (load "~/quicklisp.lisp")
    ? (quicklisp-quickstart:install)
    ? (ql:quickload "quicklisp-slime-helper")

Next time:

    ccl
    ? (load "~/quicklisp/setup.lisp")
    
### Post-freshen fix file modes (if needed)

    chmod +x ~/bin/Markdown.pl
