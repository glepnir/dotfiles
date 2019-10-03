#!/usr/bin/env bash

# Colors
ESC_SEQ="\x1b["
COL_RESET=$ESC_SEQ"39;49;00m"
COL_RED=$ESC_SEQ"31;01m"
COL_GREEN=$ESC_SEQ"32;01m"
COL_YELLOW=$ESC_SEQ"33;01m"
COL_BLUE=$ESC_SEQ"34;01m"
COL_MAGENTA=$ESC_SEQ"35;01m"
COL_CYAN=$ESC_SEQ"36;01m"

function ok() {
    echo -e "$COL_GREEN[ok]$COL_RESET "$1
}

function bot() {
    echo -e "\n$COL_GREEN\[._.]/$COL_RESET - "$1
}

function running() {
    echo -en "$COL_YELLOW ⇒ $COL_RESET"$1": "
}

function action() {
    echo -e "\n$COL_YELLOW[action]:$COL_RESET\n ⇒ $1..."
}

function warn() {
    echo -e "$COL_YELLOW[warning]$COL_RESET "$1
}

function error() {
    echo -e "$COL_RED[error]$COL_RESET "$1
}

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
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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

# Just to avoid a potential bug
mkdir -p ~/Library/Caches/Homebrew/Formula
brew doctor
# skip those GUI clients, git command-line all the way
require_brew git
# update zsh to latest
require_brew zsh
# update ruby to latest
# use versions of packages installed with homebrew
RUBY_CONFIGURE_OPTS="--with-openssl-dir=`brew --prefix openssl` --with-readline-dir=`brew --prefix readline` --with-libyaml-dir=`brew --prefix libyaml`"
require_brew ruby
# set zsh as the user login shell
CURRENTSHELL=$(dscl . -read /Users/$USER UserShell | awk '{print $2}')
if [[ "$CURRENTSHELL" != "/usr/local/bin/zsh" ]]; then
  bot "setting newer homebrew zsh (/usr/local/bin/zsh) as your shell (password required)"
  # sudo bash -c 'echo "/usr/local/bin/zsh" >> /etc/shells'
  # chsh -s /usr/local/bin/zsh
  sudo dscl . -change /Users/$USER UserShell $SHELL /usr/local/bin/zsh > /dev/null 2>&1
  ok
fi

bot "install tools..."
brew install curl
brew install wget
brew install --HEAD yabai
 brew install koekeishiya/formulae/skhd
brew install ranger
brew install emacs-mac --with-natural-title-bar --with-spacemacs-icon
ok



# ###########################################################
# install oh-my-zsh
# ###########################################################

bot "zsh setup"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
bot "creating symlinks for ranger..."
action "ln -s  ./zsh/.zshenv $HOME/.zshenv
        ln -s  ./zsh/.zshrc $HOME/.zshrc"
ln -s  ./zsh/.zshenv $HOME/.zshenv
ln -s  ./zsh/.zshrc $HOME/.zshrc
ok

# ###########################################################
# install dotfiles
# ###########################################################
bot "creating symlinks for ranger..."
action "ln -s ./ranger $HOME/.ranger"
ln -s ./ranger $HOME/.ranger
ok


# ###########################################################
# install thinkvim
# ###########################################################

bot "thinkvim install..."
read -r -p "Do you want to install thinkvim now? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  bot "Installing neovim"
  git clone https://github.com/hardcoreplayers/ThinkVim.git ~/.config/nvim
  ok "Installed thinkvim plugins by open neovim"
else
  ok "skipped. Install by running :PluginInstall within vim"
fi

# ###########################################################
# install eva-emacs
# ###########################################################

bot "Eva emacs install..."
read -r -p "Do you want to install eva-emacs now? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  bot "Installing eva-emacs"
  git clone https://github.com/hardcoreplayers/eva-emacs.git ~/.emacs.d/
  cd ~
  cd .emacs.d
  make
else
  ok "skipped install eva-emacs"
fi


brew update && brew upgrade && brew cleanup && brew cask cleanup

bot "All done"
