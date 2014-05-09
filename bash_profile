# .bash_profile

export PATH=$PATH:$HOME/bin
export EDITOR=emacs

[ -r $HOME/.bash_profile.local ] && source $HOME/.bash_profile.local
[ -r $HOME/.bashrc ] && source $HOME/.bashrc
[ -r $HOME/bin/dropbox.py ] && $HOME/bin/dropbox.py start

