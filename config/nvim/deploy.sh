#!usr/bin/env bash

oceanic_material="$HOME/.config/nvim/colors/oceanic_material.vim"

cp -R $oceanic_material $HOME/workstation/vim/oceanic-material/colors/

cd $HOME/workstation/vim/oceanic-material

read -p "Commit message:" commit_message

git add .
git commit -m $commit_message
git push


