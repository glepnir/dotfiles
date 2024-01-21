# Enviroment variables
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/var"

# Env
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$XDG_DATA_HOME/go/bin"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

PathAppend() { [ -d "$1" ] && PATH="$PATH:$1"; }

## Go
PathAppend "$XDG_DATA_HOME/go/bin"
PathAppend "/usr/local/go/bin"
# Rust
PathAppend "$CARGO_HOME/bin"
# Lua
PathAppend "$XDG_DATA_HOME/.luarocks/bin"

unset PathAppend

# Editor
export EDITOR="nvim"
export TERMINAL='kitty'

# Bat
export BAT_THEME="TwoDark"

# Fzf
export FZF_COMPLETION_TRIGGER='**'
export FZF_DEFAULT_COMMAND='rg --files --hidden'
export FZF_DEFAULT_OPTS='--height 90% --layout reverse --border --color "border:#b877db" --preview="bat --color=always {}"'
