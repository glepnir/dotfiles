#!/usr/bin/env bash

# Install neovim and thinkvim config
bot "Install neovim"
require_brew neovim
running "Configruation thinkvim"
git clone --depth=1 https://github.com/hardcoreplayers/thinkvim ~/.config/nvim

ok