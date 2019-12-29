#!/usr/bin/env bash

# include my library helpers for colorized echo and require_brew, etc
source ./lib_script/lib_func.sh

UserLocation=0
read -r -p "Are you a Chinese user? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  UserLocation=1
fi

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

###########################################################
Git Config
###########################################################

# skip those GUI clients, git command-line all the way
require_brew git

bot "OK, now I am going to update the .gitconfig for your user info:"
grep 'user = GITHUBUSER' ./gitconf/.gitconfig > /dev/null 2>&1
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

  sed -i "s/GITHUBFULLNAME/$firstname $lastname/" ./gitconf/.gitconfig > /dev/null 2>&1 | true
  if [[ ${PIPESTATUS[0]} != 0 ]]; then
    echo
    running "looks like you are using MacOS sed rather than gnu-sed, accommodating"
    sed -i '' "s/GITHUBFULLNAME/$firstname $lastname/" ./git/.gitconfig
    sed -i '' 's/GITHUBEMAIL/'$email'/' ./gitconf/.gitconfig
    sed -i '' 's/GITHUBUSER/'$githubuser'/' ./gitconf/.gitconfig
    ok
  else
    echo
    bot "looks like you are already using gnu-sed. woot!"
    sed -i 's/GITHUBEMAIL/'$email'/' ./gitconf/.gitconfig
    sed -i 's/GITHUBUSER/'$githubuser'/' ./gitconf/.gitconfig
  fi
fi

###########################################################
bot "update ruby"
###########################################################

RUBY_CONFIGURE_OPTS="--with-openssl-dir=`brew --prefix openssl` --with-readline-dir=`brew --prefix readline` --with-libyaml-dir=`brew --prefix libyaml`"
require_brew ruby

# ###########################################################
bot "zsh setup"
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

# symslink zsh config
ZSHRC="$HOME/.zshrc"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
running "Configuring zsh"
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

# ###########################################################
bot " Install Develop Tools"
# ###########################################################
require_brew curl
require_brew wget
require_brew make
require_brew ctags
require_brew gnutls
require_brew node
require_brew yarn

read -r -p "Are you a gopher? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_cask golang
  go get golang.org/x/tools/gopls@latest
  mkdir -p ~/workspace
else
  ok "skipped"
fi

read -r -p "Are you a vimer? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_brew neovim
else
  ok "skipped"
fi

read -r -p "Are you a emacser? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  brew tap daviderestivo/emacs-head
  brew install emacs-head --HEAD --with-cocoa --with-imagemagick --with-jansson
  ln -s /usr/local/opt/emacs-head/Emacs.app /Applications
  running "Configruation Emacs"
  git clone https://github.com/hardcoreplayers/supremacs ~/.config/emacs
  cd ~/.config/emacs
  make
else
  ok "skipped"
fi

# ok

# ###########################################################
bot " Install Gui Applications"
# ###########################################################

read -r -p "Do you want install iterm2? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_cask iterm2
else
  ok "skipped"
fi

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

read -r -p "Do you want install postman? [y|N] " response
if [[ $response =~ (y|yes|Y) ]];then
  require_cask postman
else
  ok "skipped"
fi

if [[ $UserLocation =~ 1 ]];then
  read -r -p "Do you want install QQ? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]];then
    require_cask visual-studio-code
  else
    ok "skipped"
  fi
  read -r -p "Do you want install wechat? [y|N] " response
  if [[ $response =~ (y|yes|Y) ]];then
    require_cask wechat
  else
    ok "skipped"
  fi
fi

brew update && brew upgrade && brew cleanup

bot "All done"
