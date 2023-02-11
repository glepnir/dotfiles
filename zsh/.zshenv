# Enviroment variables

# You may need to manually set your language environment
export LANG=en_US.UTF-8

#Homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# neovim build
export PATH="$PATH:$HOME/Workspace/neovim/build/bin/"

# Zig
export PATH="$PATH:$HOME/Workspace/zls/zig-out/bin"

# Go
export GOPATH="$HOME/.go"
export GO111MODULE="on"
export PATH="$PATH:$HOME/.go/bin"
export PATH="$PATH:/usr/local/go/bin"
export GOPROXY="https://goproxy.cn"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Cland
export PATH="/usr/local/opt/llvm/bin:$PATH"

# neovim
export VIM='/Users/joyce/Workspace/neovim'
export VIMRUNTIME='/Users/joyce/Workspace/neovim/runtime'

# Editor
export EDITOR="nvim"
export GIT_EDITOR="nvim"
export REACT_EDITOR="nvim"

# Yabai
export YABAI_CERT=yabai-cert

# Bat
export BAT_THEME="TwoDark"

# Fzf
export FZF_COMPLETION_TRIGGER='**'
export FZF_DEFAULT_COMMAND='rg --files --hidden'
export FZF_DEFAULT_OPTS='--height 90% --layout reverse --border --color "border:#b877db" --preview="bat --color=always {}"'
