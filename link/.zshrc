# alias
alias vi="nvim"
alias reload="source ~/.zshrc"

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


zinit ice depth=1 atload"!source ~/.theme.zsh" lucid nocd
zinit light romkatv/powerlevel10k

# #=== OH-MY-ZSH & PREZTO PLUGINS =======================
zinit for \
      OMZL::{'history','completion','git','grep','key-bindings'}.zsh

zinit wait lucid for \
      OMZP::{'extract','fzf','git','sudo'}

# Plugins
zinit ice depth=1 wait lucid
zinit light Aloxaf/fzf-tab

zinit ice depth=1 wait blockf lucid atpull"zinit creinstall -q ."
zinit light clarketm/zsh-completions

zinit ice depth=1 wait lucid atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay"
zinit light zdharma-continuum/fast-syntax-highlighting

zinit ice depth=1 wait lucid compile"{src/*.zsh,src/strategies/*.zsh}" atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

zinit ice depth=1 wait lucid nocd
zinit light wfxr/forgit

zinit ice depth=1 wait"2" lucid
zinit light hlissner/zsh-autopair

# set proxy
function proxy() {
  export http_proxy=http://127.0.0.1:1087
  export https_proxy=http://127.0.0.1:1087
  export ALL_PROXY=socks5://127.0.0.1:1080
  echo -e "\e[32mProxy has been successfully set.\e[0m"
}

# unset
function unproxy() {
  unset http_proxy
  unset https_proxy
  unset ALL_PROXY
  echo -e "\e[31mProxy has been unset.\e[0m"
}

# create tmux new session with window name
tn() {
  tmux new-session -d -s $1
  tmux rename-window -t $1:1 'main'
  tmux a -t $1
}

# open file
fopen() {
  IFS=$'\n' out=($(fzf --query="$1" --multi))
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-nvim} "$file"
  fi
}

# find-in-file - usage: fif <searchTerm>
fsearch() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  file=$(rg --files-with-matches --no-messages "$1" | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}")
  nvim $file
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# if [ "$TMUX" = "" ]; then tn work; fi
