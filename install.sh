#!/usr/bin/env zsh
set -e

echo 'updating submodules'
git submodule update --init --recursive

echo 'linking stuff'
local here=$(dirname $0:A)
for file in .Xdefaults .gitconfig .screenrc .slrnrc .vimrc .vim .xmobarrc .zshenv .zshrc .zaliases; do
    if [[ $file == '.gitconfig' && $USER != 'derek' ]]; then
        echo "not linking $file, it has my name in it!  do it yourself"
    else
        if [[ $(readlink -f $HOME/$file) != $(readlink -f $here/$file) ]]; then
            ln -i -s -T $here/$file $HOME/$file
            echo "linked $file"
        fi
    fi
done
