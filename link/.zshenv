# Enviroment variables
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/var"

# Env
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export LC_ALL=en_US.UTF-8

PathAppend() { [ -d "$1" ] && PATH="$PATH:$1"; }

if [ -d "$HOME/.luarocks/bin" ]; then
  PathAppend "$HOME/.luarocks/bin"
fi

if [ -d "/opt/homebrew/opt/rustup" ]; then
  PathAppend "/opt/homebrew/opt/rustup/bin"
fi

## Binary path
# Rust
PathAppend "$CARGO_HOME/bin"
# Vim Debugger Script
PathAppend "$XDG_DATA_HOME/nvim/bin"
PathAppend "$XDG_CONFIG_HOME/dotfiles/bin"

unset PathAppend
