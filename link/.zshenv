# Enviroment variables
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/var"

# Editor
export EDITOR="nvim"

# Env
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$XDG_DATA_HOME/go/bin"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

PathAppend() { [ -d "$1" ] && PATH="$PATH:$1"; }

if [ -d "$HOME/.luarocks/bin" ]; then
  PathAppend "$HOME/.luarocks/bin"
fi

# Dynamically add Python bin directories to PATH
if [ -d "$HOME/Library/Python" ]; then
  for dir in $HOME/Library/Python/*; do
    if [ -d "$dir/bin" ]; then
      PathAppend "$dir/bin"
    fi
  done
fi

## Binary path
PathAppend "$XDG_DATA_HOME/go/bin"
# Rust
PathAppend "$CARGO_HOME/bin"
# Vim Debugger Script
PathAppend "$XDG_DATA_HOME/nvim/bin"

unset PathAppend
