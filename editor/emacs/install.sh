#!/usr/bin/env bash

# Install Emacs27 and supremacs config
bot "Install Emacs27"
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --with-cocoa --with-imagemagick --with-jansson
ln -s /usr/local/opt/emacs-head/Emacs.app /Applications
running "Configruation Emacs"
git clone https://github.com/hardcoreplayers/supremacs ~/.config/emacs
cd ~/.config/emacs
make