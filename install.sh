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

bot "OK, now I am going to update the .gitconfig for your user info:"

gitfile="$HOME/.gitconfig"
running "link .gitconfig"
if [ ! -f "gitfile" ]; then
  read -r -p "Seems like your gitconfig file exist,do you want delete it? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]]; then
    rm -rf $HOME/.gitconfig
    action "cp ./git/.gitconfig ~/.gitconfig"
    sudo cp $HOME/.dotfiles/git/.gitconfig  $HOME/.gitconfig
    ln -s $HOME/.dotfiles/git/.gitignore  $HOME/.gitignore
    ok
  else
    ok "skipped"
  fi
fi
grep 'user = GITHUBUSER'  $HOME/.gitconfig > /dev/null 2>&1
if [[ $? = 0 ]]; then
    read -r -p "What is your git username? " githubuser

  fullname=`osascript -e "long user name of (system info)"`

  if [[ -n "$fullname" ]];then
    lastname=$(echo $fullname | awk '{print $2}');
    firstname=$(echo $fullname | awk '{print $1}');
  fi

  if [[ -z $lastname ]]; then
    lastname=`dscl . -read /Users/$(whoami) | grep LastName | sed "s/LastName: //"`
  fi
  if [[ -z $firstname ]]; then
    firstname=`dscl . -read /Users/$(whoami) | grep FirstName | sed "s/FirstName: //"`
  fi
  email=`dscl . -read /Users/$(whoami)  | grep EMailAddress | sed "s/EMailAddress: //"`

  if [[ ! "$firstname" ]]; then
    response='n'
  else
    echo  "I see that your full name is $COL_YELLOW$firstname $lastname$COL_RESET"
    read -r -p "Is this correct? [Y|n] " response
  fi

  if [[ $response =~ ^(no|n|N) ]]; then
    read -r -p "What is your first name? " firstname
    read -r -p "What is your last name? " lastname
  fi
  fullname="$firstname $lastname"

  bot "Great $fullname, "

  if [[ ! $email ]]; then
    response='n'
  else
    echo  "The best I can make out, your email address is $COL_YELLOW$email$COL_RESET"
    read -r -p "Is this correct? [Y|n] " response
  fi

  if [[ $response =~ ^(no|n|N) ]]; then
    read -r -p "What is your email? " email
    if [[ ! $email ]];then
      error "you must provide an email to configure .gitconfig"
      exit 1
    fi
  fi


  running "replacing items in .gitconfig with your info ($COL_YELLOW$fullname, $email, $githubuser$COL_RESET)"

  # test if gnu-sed or MacOS sed

  sed -i "s/GITHUBFULLNAME/$firstname $lastname/" ./git/.gitconfig > /dev/null 2>&1 | true
  if [[ ${PIPESTATUS[0]} != 0 ]]; then
    echo
    running "looks like you are using MacOS sed rather than gnu-sed, accommodating"
    sed -i '' "s/GITHUBFULLNAME/$firstname $lastname/"  $HOME/.gitconfig
    sed -i '' 's/GITHUBEMAIL/'$email'/'  $HOME/.gitconfig
    sed -i '' 's/GITHUBUSER/'$githubuser'/'  $HOME/.gitconfig
    ok
  else
    echo
    bot "looks like you are already using gnu-sed. woot!"
    sed -i 's/GITHUBEMAIL/'$email'/'  $HOME/.gitconfig
    sed -i 's/GITHUBUSER/'$githubuser'/'  $HOME/.gitconfig
  fi
fi

# ###########################################################
bot "zsh setup"
# ###########################################################

require_brew zsh

# symslink zsh config
ZSHRC="$HOME/.zshrc"
running "Configuring zsh"
if [ ! -f "ZSHRC" ]; then
  read -r -p "Seems like your zshrc file exist,do you want delete it? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]]; then
    rm -rf $HOME/.zshrc
    rm -rf $HOME/.zshenv
    action "link zsh/.zshrc and zsh/.zshenv"
    ln -s  $HOME/.dotfiles/zsh/zshenv $HOME/.zshenv
    ln -s  $HOME/.dotfiles/zsh/zshrc $HOME/.zshrc
    ln -s  $HOME/.dotfiles/zsh/p10k.zsh $HOME/.p10k.zsh
    source ~/.zshrc
  else
    ok "skipped"
  fi
fi

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

action "link tmux conf"
ln -s  $HOME/.dotfiles/tmux/.tmux.conf $HOME/.tmux.conf
ok

action "link .rgignore"
ln -s  $HOME/.dotfiles/.rgignore $HOME/.rgignore
ok

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

bot "link clang-format"
ln -s ~/.dotfiles/.clang-format ~/.clang-format


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
