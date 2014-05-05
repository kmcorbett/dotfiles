# .bash_profile

export PATH=$PATH:$HOME/bin

[ -r $HOME/.bash_profile.local ] && source $HOME/.bash_profile.local
[ -r $HOME/.bashrc ] && source $HOME/.bashrc

# DropBox daemon
DROPBOXD=$HOME/.dropbox-dist/dropboxd
[ -f $DROPBOXD ] && $DROPBOXD

export EDITOR=emacs
