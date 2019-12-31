# Enviroment variables

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Neovim
export PATH=$HOME/neovim/bin:$PATH

# Zsh
export ZSH="$HOME/.oh-my-zsh"
source "$ZSH/oh-my-zsh.sh"

#Homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# Go
export GOPATH="$HOME/workspace"
export GO111MODULE="on"
export PATH=$PATH:$HOME/workstation/bin
export PATH=$PATH:/usr/local/go/bin
export GOPROXY=https://goproxy.io

# Node
export PATH=$PATH:$HOME/.node_global/bin
export PATH=$PATH:$HOME/.yarn_global/bin

# Rust
export PATH=$PATH:$HOME/.cargo/bin

# Editor
export EDITOR="nvim"
export GIT_EDITOR="nvim"
export REACT_EDITOR="nvim"
