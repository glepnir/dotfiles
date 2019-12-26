#!/usr/bin/env bash

# include my library helpers for colorized echo and require_brew, etc
source ./lib_script/lib_func.sh

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

# ###########################################################
# Git Config
# ###########################################################

# # skip those GUI clients, git command-line all the way
# require_brew git

# bot "OK, now I am going to update the .gitconfig for your user info:"
# grep 'user = GITHUBUSER' ./gitconf/.gitconfig > /dev/null 2>&1
# if [[ $? = 0 ]]; then
#     read -r -p "What is your git username? " githubuser

#   fullname=`osascript -e "long user name of (system info)"`

#   if [[ -n "$fullname" ]];then
#     lastname=$(echo $fullname | awk '{print $2}');
#     firstname=$(echo $fullname | awk '{print $1}');
#   fi

#   if [[ -z $lastname ]]; then
#     lastname=`dscl . -read /Users/$(whoami) | grep LastName | sed "s/LastName: //"`
#   fi
#   if [[ -z $firstname ]]; then
#     firstname=`dscl . -read /Users/$(whoami) | grep FirstName | sed "s/FirstName: //"`
#   fi
#   email=`dscl . -read /Users/$(whoami)  | grep EMailAddress | sed "s/EMailAddress: //"`

#   if [[ ! "$firstname" ]]; then
#     response='n'
#   else
#     echo  "I see that your full name is $COL_YELLOW$firstname $lastname$COL_RESET"
#     read -r -p "Is this correct? [Y|n] " response
#   fi

#   if [[ $response =~ ^(no|n|N) ]]; then
#     read -r -p "What is your first name? " firstname
#     read -r -p "What is your last name? " lastname
#   fi
#   fullname="$firstname $lastname"

#   bot "Great $fullname, "

#   if [[ ! $email ]]; then
#     response='n'
#   else
#     echo  "The best I can make out, your email address is $COL_YELLOW$email$COL_RESET"
#     read -r -p "Is this correct? [Y|n] " response
#   fi

#   if [[ $response =~ ^(no|n|N) ]]; then
#     read -r -p "What is your email? " email
#     if [[ ! $email ]];then
#       error "you must provide an email to configure .gitconfig"
#       exit 1
#     fi
#   fi


#   running "replacing items in .gitconfig with your info ($COL_YELLOW$fullname, $email, $githubuser$COL_RESET)"

#   # test if gnu-sed or MacOS sed

#   sed -i "s/GITHUBFULLNAME/$firstname $lastname/" ./gitconf/.gitconfig > /dev/null 2>&1 | true
#   if [[ ${PIPESTATUS[0]} != 0 ]]; then
#     echo
#     running "looks like you are using MacOS sed rather than gnu-sed, accommodating"
#     sed -i '' "s/GITHUBFULLNAME/$firstname $lastname/" ./git/.gitconfig
#     sed -i '' 's/GITHUBEMAIL/'$email'/' ./gitconf/.gitconfig
#     sed -i '' 's/GITHUBUSER/'$githubuser'/' ./gitconf/.gitconfig
#     ok
#   else
#     echo
#     bot "looks like you are already using gnu-sed. woot!"
#     sed -i 's/GITHUBEMAIL/'$email'/' ./gitconf/.gitconfig
#     sed -i 's/GITHUBUSER/'$githubuser'/' ./gitconf/.gitconfig
#   fi
# fi

# ###########################################################
# update ruby to latest
# use versions of packages installed with homebrew
# ###########################################################

RUBY_CONFIGURE_OPTS="--with-openssl-dir=`brew --prefix openssl` --with-readline-dir=`brew --prefix readline` --with-libyaml-dir=`brew --prefix libyaml`"
require_brew ruby

# ###########################################################
# Update zsh and install oh-my-zsh
# ###########################################################

require_brew zsh
# set zsh as the user login shell
CURRENTSHELL=$(dscl . -read /Users/$USER UserShell | awk '{print $2}')
if [[ "$CURRENTSHELL" != "/usr/local/bin/zsh" ]]; then
  bot "setting newer homebrew zsh (/usr/local/bin/zsh) as your shell (password required)"
  # sudo bash -c 'echo "/usr/local/bin/zsh" >> /etc/shells'
  # chsh -s /usr/local/bin/zsh
  sudo dscl . -change /Users/$USER UserShell $SHELL /usr/local/bin/zsh > /dev/null 2>&1
  ok
fi

# ###########################################################
# symslink zsh config
# ###########################################################

bot "zsh setup"
ZSHRC="$HOME/.zshrc"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
bot "creating symlinks for zsh config..."
if [ ! -f "ZSHRC" ]; then
  read -r -p "Seems like your zshrc file exist,do you want delete it? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]]; then
    rm -rf $HOME/.zshrc
    rm -rf $HOME/.zshenv
    action "link zsh/.zshrc and zsh/.zshenv"
    ln -s  $HOME/.dotfiles/zsh/.zshenv $HOME/.zshenv
    ln -s  $HOME/.dotfiles/zsh/.zshrc $HOME/.zshrc
    ok "When you restart terminal it will auto install zplug and plugins"
  else
    ok "skipped"
  fi
fi

# bot "install tools..."
# brew install curl
# brew install wget
# # brew install emacs-mac --with-natural-title-bar --with-spacemacs-icon
# ok

# ###########################################################
# Install Gui Application
# ###########################################################

read -r -p "Install google-chrome? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  bot "Installing Google Chrome"
  require_cask google-chrome
else
  ok "skipped"
fi


brew update && brew upgrade && brew cleanup

bot "All done"
