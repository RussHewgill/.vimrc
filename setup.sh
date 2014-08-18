#!/bin/bash

ln -s ~/gitrepos/Configs/vim/.vimrc ~/.vimrc

ln -s ~/gitrepos/Configs/.zshrc ~/.zshrc

ln -s ~/gitrepos/Configs/.zpreztorc ~/.zpreztorc

if [ ! -d "$HOME/.vim" ]; then
    mkdir ~/.vim 
fi

if [ ! -d "$HOME/.vim" ]; then
    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim 
fi




