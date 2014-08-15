#!/bin/bash

mv ~/.vimrc{,.bak}
ln -rs vim/.vimrc ~/.vimrc

mv ~/.zshrc{,.bak}
ln -rs .zshrc ~/.zshrc

if [ ! -d "$HOME/.vim" ]; then
    mkdir ~/.vim 
fi

if [ ! -d "$HOME/.vim" ]; then
    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim 
    yaourt --noconfirm prezto-git
fi





