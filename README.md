# kmcorbett/dotfiles

## Fresh dotfiles installation

Note: All shell commands are prefaced with the prompt "$". All the
commands shown here must be run in the user's home directory. In other
words, we assume here that the user has used "cd" to set their working
directory to the default (per-user) home directory path.

### Environment

    $ export TMPDIR="~/tmp"

### Archive un-fresh files

The "fresh" command may complain if you attempt to freshen a file that
already exists and is not a link to the fresh source file.

    $ tar cvf dotfiles-archive.tar [file...]
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
