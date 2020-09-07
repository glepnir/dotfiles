# Enviroment variables

# You may need to manually set your language environment
export LANG=en_US.UTF-8

#Homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# Go
export GOPATH="$HOME/workspace"
export GO111MODULE="on"
export PATH=$PATH:$HOME/workspace/bin
export PATH=$PATH:$HOME/workstation/vim/magit-nvim/bin
export PATH=$PATH:/usr/local/go/bin
export GOPROXY=https://goproxy.cn

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Editor
export EDITOR="nvim"
export GIT_EDITOR="nvim"
export REACT_EDITOR="nvim"

# Yabai
export YABAI_CERT=yabai-cert
