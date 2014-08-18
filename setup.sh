#!/bin/bash

ln -s ~/gitrepos/Configs/vim/.vimrc ~/.vimrc

ln -s ~/Desktop/Configs/.zshrc ~/.zshrc

ln -s ~/Desktop/Configs/.zpreztorc ~/.zpreztorc

if [ ! -d "$HOME/.vim" ]; then
    mkdir ~/.vim 
fi

if [ ! -d "$HOME/.vim" ]; then
    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim 
fi




