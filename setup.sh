#!/bin/bash

install='aptitude install -y'

aptitude update

if ! hash git 2>/dev/null; then
  $install git; fi

if ! hash zsh 2>/dev/null; then
  $install zsh; fi

if ! hash vim 2>/dev/null; then
  $install vim; fi

mkdir ~/gitrepos
cd ~/gitrepos
git clone https://github.com/russhewgill/configs

if [[ -f ~/.vimrc ]]; then
  mv ~/.vimrc{,.bak}; fi
ln -s ~/gitrepos/configs/vim/.vimrc ~/.vimrc

if [[ -f ~/.zshrc ]]; then
  mv ~/.zshrc{,.bak}; fi
ln -s ~/gitrepos/configs/.zshrc ~/.zshrc

if [[ -f ~/.zpreztorc ]]; then
  mv ~/.zpreztorc{,.bak}; fi
ln -s ~/gitrepos/configs/.zpreztorc ~/.zpreztorc

mkdir -p ~/.vim/bundle
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim -e +PluginInstall +qa

echo 'git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"; setopt EXTENDED_GLOB; for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"; done; : > ~/.zlogout' | zsh

chsh -s $(which zsh) $(cat /etc/passwd | tail -n 1 | awk -F: '{print $1}')

