# alias
alias vi="nvim"
alias reload="source ~/.zshrc"
#alias ls="lsd"
alias ll="ls -l"
alias la="ls -la"
alias lt="ls --tree"
alias proxy="export http_proxy=http://127.0.0.1:1087;export https_proxy=http://127.0.0.1:1087;export ALL_PROXY=socks5://127.0.0.1:1080"
alias unproxy="unset http_proxy;unset https_proxy;unset ALL_PROXY"

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
	print -P "%F{33}▓▒░ %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
	command mkdir -p $HOME/.zinit && command chmod g-rwX "$HOME/.zinit"
	command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
		print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
		print -P "%F{160}▓▒░ The clone has failed.%f"
fi
source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# if [ -z "$TMUX" ]
# then
#     tmux attach -t work || tmux new -s work
# fi

zinit ice depth=1 atload"!source ~/.p10k-evilball.zsh" lucid nocd
zinit light romkatv/powerlevel10k

# #=== OH-MY-ZSH & PREZTO PLUGINS =======================
zinit for \
      OMZL::{'history','completion','git','grep','key-bindings'}.zsh

zinit wait lucid for \
      OMZP::{'colored-man-pages','extract','fzf','git','sudo','golang'}

# Plugins
zinit ice depth=1 wait lucid
zinit light Aloxaf/fzf-tab

zinit ice depth=1 wait blockf lucid atpull"zinit creinstall -q ."
zinit light clarketm/zsh-completions

zinit ice depth=1 wait lucid compile"{src/*.zsh,src/strategies/*.zsh}" atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

zinit ice depth=1 wait"2" lucid light-mode for \
      {'wfxr/forgit','hlissner/zsh-autopair','MichaelAquilina/zsh-you-should-use'}

# open file
fo() {
  #IFS=$'\n' out=("$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)")
  IFS=$'\n' out=($(fzf --query="$1" --multi))
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-nvim} "$file"
  fi
}

# cd directory and open file can pass word
fcd() {
  local dir
  dir=$(fd --hidden --type d "$1" . $HOME | fzf --preview 'tree -C {}' +m) && cd "$dir"
}

# cd directory and open file can pass word
co() {
  local dir
  dir=$(fd --hidden --type d "$1" . $HOME | fzf --preview 'tree -C {}' +m) && cd "$dir" && fo
}

# find-in-file - usage: fif <searchTerm>
fif() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  rg --files-with-matches --no-messages "$1" | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
