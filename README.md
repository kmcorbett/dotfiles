dotfiles
========

my fresh dot files

save old dot files

``` 
cd 
files=".bash_profile .bashrc .gitconfig .emacs.d/init.el .ssh/config .tmux.conf"
tar cvf dotfiles-archive.tar $files
```

cd
git clone ssh://git@github.com/kmcorbett/dotfiles .dotfiles
ln -s ~/.dotfiles/freshrc ~/.freshrc
bash -c "`curl -sL get.freshshell.com`"
fresh
```

