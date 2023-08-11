#!/usr/bin/env bash

# include my library helpers for colorized echo and require_brew, etc
source ./util.sh

UserLocation=0
read -r -p "Are you a Chinese user? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  UserLocation=1
fi

# @see https://apple.stackexchange.com/questions/10467/how-to-increase-keyboard-key-repeat-rate-on-os-x
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10

# ###########################################################
# Install non-brew various tools (PRE-BREW Installs)
# ###########################################################
bot "ensuring build/install tools are available"
if ! xcode-select --print-path &> /dev/null; then
    # Prompt user to install the XCode Command Line Tools
    xcode-select --install &> /dev/null

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # Wait until the XCode Command Line Tools are installed
    until xcode-select --print-path &> /dev/null; do
        sleep 5
    done

    print_result $? ' XCode Command Line Tools Installed'

    # Prompt user to agree to the terms of the Xcode license
    # https://github.com/alrra/dotfiles/issues/10

    sudo xcodebuild -license
    print_result $? 'Agree with the XCode Command Line Tools licence'

fi

# ###########################################################
# install homebrew (CLI Packages)
# ###########################################################

running "checking homebrew..."
brew_bin=$(which brew) 2>&1 > /dev/null
if [[ $? != 0 ]]; then
  action "installing homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  if [[ $? != 0 ]]; then
    error "unable to install homebrew, script $0 abort!"
    exit 2
  fi
else
  ok
  bot "Homebrew"
  read -r -p "run brew update && upgrade? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]]; then
    action "updating homebrew..."
    brew update
    ok "homebrew updated"
    action "upgrading brew packages..."
    brew upgrade
    ok "brews upgraded"
  else
    ok "skipped brew package upgrades."
  fi
fi

brew doctor

###########################################################
# Git Config
###########################################################

# skip those GUI clients, git command-line all the way
action "install the latest version of git"
require_brew git

# ###########################################################
bot "zsh setup"
# ###########################################################

require_brew zsh

shopt -s dotglob
for file in "$HOME/.dotfiles/link"/*; do
  if [ -f "$file" ]; then
    filename=$(basename "$file")
    ln -s "$file" "$HOME/$filename"
    echo "Created symlink: $HOME/$filename -> $file"
  fi
done
shopt -u dotglob

git config --global user.name "glepnir"
git config --global user.email "glephunter@gmail.com"
git config --global core.editor "nvim"

# ###########################################################
bot "Install fonts"
# ###########################################################
read -r -p "Install fonts? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  bot "installing fonts"
  sh ./fonts/install.sh
  ok
  brew tap homebrew/cask-fonts
  brew install font-iosevka
fi

# ###########################################################
bot " Install Develop Tools"
# ###########################################################
require_brew ripgrep
require_brew bat
require_brew make
require_brew tmux
require_brew fzf
/usr/local/opt/fzf/install
brew install jesseduffield/lazygit/lazygit
require_cask docker

## llvm with cland
require_brew llvm

action "Install tpm"
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ok "when you open tmux,you must type prefix {default: Ctrl+space } + I to install tmux plugins"

require_brew node
require_brew yarn

require_brew lua
require_brew luarocks
luarocks install vusted
require_brew ninja
require_brew zls
ok

action "Install yabai and skhd"
brew install koekeishiya/formulae/yabai
brew install koekeishiya/formulae/skhd
sudo yabai --install-sa
sudo yabai --load-sa
ln -s "${HOME}/.dotfiles/yabai/yabairc" "${HOME}/.yabairc"
ln -s "${HOME}/.dotfiles/yabai/skhdrc" "${HOME}/.skhdrc"
yabai --start-service
skhd --start-service

# rust
require_brew rustup-init
rustup-init

# zig
require_brew zig
# go
require_brew golang

bot "Install neovim"
require_brew neovim --HEAD
running "Configruation nvim"
git clone https://github.com/glepnir/nvim ~/.config/nvim
ok

bot "install develop"
require_brew gopls
requier_brew rust-analyzer
require_brew lua-language-server
require_brew clang-format
require_brew stylua
require_brew helix

npm i -g eslint
npm i -g typescript
npm i -g typescript-language-server
npm i -g prettier
npm i -g vscode-langservers-extracted
npm i -g bash-language-server
npm i -g vite

# ###########################################################
bot " Install Gui Applications"
# ###########################################################
require_brew raycast

read -r -p "Do you want install kitty? [y|N] " responseinstall
if [[ $response =~ (y|yes|Y) ]];then
  require_cask kitty
  ln -s ~/.dotfiles/config/kitty ~/.config/kitty
else
  ok "skipped"
fi

require_cask licecap

read -r -p "Do you want install google-chrome? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_cask google-chrome
else
  ok "skipped"
fi

read -r -p "Do you want install vscode? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_cask visual-studio-code
else
  ok "skipped"
fi

if [[ $UserLocation =~ 1 ]];then
  read -r -p "Do you want install QQ? [y|N] " qqresponse
  if [[ $qqresponse =~ (y|yes|Y) ]];then
    require_cask qq
  else
    ok "skipped"
  fi
  read -r -p "Do you want install wechat? [y|N] " wxresponse
  if [[ $wxresponse =~ (y|yes|Y) ]];then
    require_cask wechat
  else
    ok "skipped"
  fi
fi

brew update && brew upgrade && brew cleanup

bot "All done"
