# .bash_profile

# Source init files
[ -r $HOME/.bashrc ] && source $HOME/.bashrc
[ -r $HOME/.bashrc.local ] && source $HOME/.bashrc.local

# Path for shell user

[ -e $HOME/bin ] && export PATH=$PATH:$HOME/bin

