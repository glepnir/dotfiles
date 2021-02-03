# Enviroment variables

# You may need to manually set your language environment
export LANG=en_US.UTF-8

#Homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# Zig
export PATH="$HOME/zig/build/bin:$PATH"
export PATH="$HOME/zls/zig-cache/bin:$PATH"

# Go
export GOPATH="$HOME/workstation/go"
export GO111MODULE="on"
export PATH=$PATH:$HOME/workstation/go/bin
export PATH=$PATH:/usr/local/go/bin
export GOPROXY=https://goproxy.cn

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Cland
export PATH="/usr/local/opt/llvm/bin:$PATH"

# Editor
export EDITOR="nvim"
export GIT_EDITOR="nvim"
export REACT_EDITOR="nvim"

# Yabai
export YABAI_CERT=yabai-cert
