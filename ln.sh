#!/bin/bash

if [[ -f ~/.vimrc ]]; then
  mv ~/.vimrc{,.bak}; fi
ln -s ~/gitrepos/configs/vim/.vimrc ~/.vimrc

if [[ -f ~/.zshrc ]]; then
  mv ~/.zshrc{,.bak}; fi
ln -s ~/gitrepos/configs/.zshrc ~/.zshrc

if [[ -f ~/.zpreztorc ]]; then
  mv ~/.zpreztorc{,.bak}; fi
ln -s ~/gitrepos/configs/.zpreztorc ~/.zpreztorc

if [[ -d ~/.config ]]; then
  mkdir ~/.config; fi 
if [[ -f ~/.config/.aliases ]]; then
  mv ~/.config/.aliases{,.bak}; fi
ln -s ~/gitrepos/configs/.aliases ~/.config/.aliases

