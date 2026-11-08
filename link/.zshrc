if [[ -z $TMUX && -t 0 && -t 1 ]]; then
  exec tmux new-session -A -s work -n main
fi

export EDITOR=nvim
export VISUAL=nvim

# alias
alias vi="nvim"
alias reload="exec zsh"
alias gcmsg="git commit -m"
alias gl="git pull"
alias gp="git push --force"
alias gpf="git push --force-with-lease --force-if-includes"
alias vv="VIMRUNTIME=~/workspace/neovim ~/workspace/build/bin/nvim"

autoload -Uz add-zsh-hook

# Git status segment (runs in the background, prints nothing outside a work tree)
prompt_git_status() {
  local st
  st=$(git --no-optional-locks status --porcelain=v2 --branch 2>/dev/null) || return

  local -a line parts upstream_divergence working_info
  local head commit_hash ahead behind conflicts staged changed untracked

  while read -rA line; do
    case "$line" in
      '# branch.oid'*)
        [[ $line[3] != "(initial)" ]] && commit_hash=${line[3]:0:7}
        ;;
      '# branch.head'*) # Current branch
        head=$line[3]
        [[ $head == "(detached)" ]] && head="detached"
        ;;
      '# branch.ab'*) # Divergence from upstream
        ahead=${line[3]#+}
        behind=${line[4]#-}
        ;;
      (1|2)*) # Modified or renamed/copied
        [[ ${${line[2]}[1]} != "." ]] && (( staged++ ))
        [[ ${${line[2]}[2]} != "." ]] && (( changed++ ))
        ;;
      'u'*) # Unmerged
        (( conflicts++ ))
        ;;
      '?'*) # Untracked
        (( untracked++ ))
        ;;
    esac
  done <<< "$st"

  # Escape % so branch names can't inject prompt sequences
  parts+="%F{8}${head//\%/%%}%f"
  [[ -n $commit_hash ]] && parts+="%F{cyan}$commit_hash%f"

  (( ahead > 0 ))  && upstream_divergence+="%F{blue}↑$ahead%f"
  (( behind > 0 )) && upstream_divergence+="%F{blue}↓$behind%f"
  (( $#upstream_divergence )) && parts+="${(j::)upstream_divergence}"

  (( conflicts > 0 )) && working_info+="%F{red}×$conflicts%f"
  (( staged > 0 ))    && working_info+="%F{green}●$staged%f"
  (( changed > 0 ))   && working_info+="%F{208}✻$changed%f"
  (( untracked > 0 )) && working_info+="%F{yellow}+$untracked%f"

  if (( $#working_info )); then
    parts+="${(j::)working_info}"
  else
    parts+="%F{green}✔%f"
  fi

  print -rn -- "${(j: :)parts}"
}

# Build the prompt; $1 is the git segment (may be empty)
prompt_git_define_prompt() {
  setopt localoptions extendedglob

  local -a parts

  # Current working directory (%~ abbreviates $HOME and named dirs, safe with %)
  parts+="%F{240}in %F{blue}%~%f"
  # Fish-style abbreviated path alternative:
  # parts+="%F{green}in %F{blue}${${PWD/#$HOME/~}//(#b)([^\/])[^\/][^\/]#\//$match[1]/}%f"

  # Git info (filled in asynchronously)
  [[ -n $1 ]] && parts+="$1"

  # Prompt arrow on its own line (red for non-zero status)
  PROMPT="${(j: :)parts}"$'\n'"%(?.%F{green}.%F{red})λ%f "
}

# Run before each prompt
prompt_git_precmd() {
  typeset -g _prompt_git_fd _prompt_git_cache _prompt_git_cache_pwd

  # Keep showing the last git info for this directory until the new one arrives
  [[ $PWD == "$_prompt_git_cache_pwd" ]] || _prompt_git_cache=
  prompt_git_define_prompt "$_prompt_git_cache"

  # Cancel a job still pending from the previous prompt
  if [[ -n $_prompt_git_fd ]]; then
    zle -F $_prompt_git_fd
    exec {_prompt_git_fd}<&-
  fi

  exec {_prompt_git_fd}< <(prompt_git_status)
  zle -F $_prompt_git_fd prompt_git_response
}

# Handle the async result
prompt_git_response() {
  local fd=$1 out
  IFS= read -r -d '' -u $fd out

  zle -F $fd
  exec {fd}<&-
  unset _prompt_git_fd

  _prompt_git_cache=$out
  _prompt_git_cache_pwd=$PWD
  prompt_git_define_prompt "$out"
  zle reset-prompt
}

add-zsh-hook precmd prompt_git_precmd

# set proxy
proxy() {
  export http_proxy=http://127.0.0.1:7890
  export https_proxy=http://127.0.0.1:7890
  export all_proxy=socks5://127.0.0.1:7891
  export no_proxy="localhost,127.0.0.1,::1"

  export HTTP_PROXY=$http_proxy
  export HTTPS_PROXY=$https_proxy
  export ALL_PROXY=$all_proxy
  export NO_PROXY=$no_proxy
  # print -P "%F{green}Proxy has been successfully set.%f"
}

# unset proxy
unproxy() {
  unset http_proxy https_proxy all_proxy no_proxy
  unset HTTP_PROXY HTTPS_PROXY ALL_PROXY NO_PROXY
  print -P "%F{red}Proxy has been unset.%f"
}

# enable proxy
proxy

# Create or attach to a tmux session (switches client when already inside tmux)
tn() {
  (( $# )) || { print -u2 "usage: tn <session>"; return 1; }
  if [[ -n $TMUX ]]; then
    tmux has-session -t "=$1" 2>/dev/null || tmux new-session -d -s "$1" -n main
    tmux switch-client -t "=$1"
  else
    tmux new-session -A -s "$1" -n main
  fi
}

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

# #=== OH-MY-ZSH & PREZTO PLUGINS =======================
zinit for \
      OMZL::{'history','completion','grep','key-bindings'}.zsh

zinit wait lucid for \
      OMZP::{'extract'}

# Plugins
zinit ice depth=1 wait lucid
zinit light Aloxaf/fzf-tab

zinit ice depth=1 wait lucid compile"{src/*.zsh,src/strategies/*.zsh}" atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

zinit ice depth=1 wait"2" lucid
zinit light hlissner/zsh-autopair

zinit ice depth=1 wait lucid
zinit light wfxr/forgit

# Full completion check at most once a day, otherwise load the dump directly
autoload -Uz compinit
() {
  if (( $# )); then
    compinit && touch "$1"
  else
    compinit -C
  fi
} ${ZDOTDIR:-$HOME}/.zcompdump(N.mh+24)

zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup

# fzf key bindings and completion (fzf 0.48+)
(( $+commands[fzf] )) && source <(fzf --zsh)

# fzf relate config
export FZF_DEFAULT_COMMAND='fd --type file --follow'
export FZF_DEFAULT_OPTS='
  --height 60%
  --border sharp
  --layout reverse
  --prompt "∷ "
  --marker ⇒
'

# fuzzy open - enter to edit, ctrl-o to open
fo() {
  local -a out
  out=("${(@f)$(fzf --query="$1" --multi --expect=ctrl-o)}")
  (( $#out >= 2 )) || return   # aborted or nothing selected
  local key=$out[1]
  shift out
  if [[ $key == ctrl-o ]]; then
    open "${out[@]}"
  else
    ${EDITOR:-nvim} "${out[@]}"
  fi
}

# find-in-file - usage: fif <searchTerm>
# Pick a matching line, preview it with bat, open nvim at that line
fif() {
  (( $# )) || { print -u2 "Need a string to search for!"; return 1; }
  local sel
  sel=$(rg --line-number --no-heading --color=always --smart-case -- "$1" |
        fzf --ansi --delimiter : \
            --preview 'bat --color=always {1} --highlight-line {2}' \
            --preview-window '+{2}/2')
  [[ -n $sel ]] && nvim "+${${sel#*:}%%:*}" "${sel%%:*}"
}
