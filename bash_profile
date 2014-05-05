# .bash_profile

export PATH=$PATH:$HOME/bin

[ -r $HOME/.bashrc ] && source $HOME/.bashrc
[ -r $HOME/.bashrc.local ] && source $HOME/.bashrc.local

# DropBox daemon
DROPBOXD=$HOME/.dropbox-dist/dropboxd
[ -f $DROPBOXD ] && $DROPBOXD
