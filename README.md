dotfiles
========

my fresh dot files

save old dot files

``` 
cd 
files=".bash_profile .bashrc .gitconfig .emacs.d/init.el .tmux.conf"
tar cvf dotfiles-archive.tar $files
rm $files
```

cd
git clone ssh://git@github.com/kmcorbett/dotfiles .dotfiles
ln -s ~/.dotfiles/freshrc ~/.freshrc
bash -c "`curl -sL --insecure get.freshshell.com`"
fresh
```

