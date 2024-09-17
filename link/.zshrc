# alias
alias vi="nvim"
alias reload="source ~/.zshrc"

# for osx
if [ -d "/opt/homebrew/opt/llvm" ]; then
  export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
  export CXX="/opt/homebrew/opt/llvm/bin/clang++"
  export CC="/opt/homebrew/opt/llvm/bin/clang"
  alias g++=$CXX
  alias gcc=$CC
  # remove duplicate /opt/llvm/bin
  export PATH=$(echo "$PATH" | sed 's|:/opt/homebrew/opt/llvm/bin||')
  export PATH="/opt/homebrew/bin:$PATH"
fi

autoload -U add-zsh-hook
# Function to get Git status
prompt_git_status() {
  git rev-parse --git-dir >&- 2>&- || {
    echo -n $'\0'
    return
  }

  local -a parts
  local fd line head ahead behind conflicts staged changed untracked commithash

  exec {fd}< <(git status --porcelain=v2 --branch)

  while read -A -u $fd line; do
    case "$line" in
      '# branch.oid'*)
        if [[ "${line[3]}" != "(initial)" ]]; then
          commit_hash="${line[3]:0:7}"
        fi
        ;;
      '# branch.head'*) # Current branch
        head="$line[3]"
        [[ $head == "(detached)" ]] && head="$(echo ":$(git rev-parse --short HEAD)")"
        ;;
      '# branch.ab'*) # Divergence from upstream
        ahead="${line[3]/#+}"
        behind="${line[4]/#-}"
        ;;
      (1|2)*) # Modified or renamed/copied
        [[ "${${line[2]}[1]}" != "." ]] && ((staged++))
        [[ "${${line[2]}[2]}" != "." ]] && ((changed++))
        ;;
      'u'*) # Unmerged
        ((conflicts++))
        ;;
      '?'*) # Untracked
        ((untracked++))
        ;;
    esac
  done

  exec {fd}<&-

  parts+="%F{8}$head%f"
  if [[ -n "$commit_hash" ]]; then
    parts+="%F{magenta}$commit_hash%f"
  fi
  local -a upstream_divergence

  [[ $ahead > 0 ]] && upstream_divergence+="%F{blue}↑$ahead%f"
  [[ $behind > 0 ]] && upstream_divergence+="%F{blue}↓$behind%f"

  if [[ $#upstream_divergence > 0 ]]; then
    parts+="${(j::)upstream_divergence}"
  fi

  local -a working_info

  [[ $conflicts > 0 ]] && working_info+="%F{red}×$conflicts%f"
  [[ $staged > 0 ]] && working_info+="%F{green}●$staged%f"
  [[ $changed > 0 ]] && working_info+="%F{208}✻$changed%f"
  [[ $untracked > 0 ]] && working_info+="%F{red}+$untracked%f"

  if [[ $#working_info > 0 ]]; then
    parts+="${(j::)working_info}"
  else
    parts+="%F{green}✔%f"
  fi

  echo -n "${(j: :)parts}"
}

# Function to define the prompt
prompt_git_define_prompt() {
  setopt localoptions extendedglob

  local -a parts=()

  # Abbreviated current working directory
  # parts+="%F{green}in %F{blue}${${PWD/#$HOME/~}//(#b)([^\/])[^\/][^\/]#\//$match[1]/}%f"
  parts+="%F{green}in %F{blue}${${PWD/#$HOME/~}}%f"

  # Git info (loaded async)
  if [[ "$1" != $'\0' ]]; then
    if [[ -n "$1" ]]; then
      parts+="$1"
    # else
    #   parts+="..."
    fi
  fi

  # Prompt arrow (red for non-zero status)
  parts+="%(?.%F{8}.%F{red})
%F{cyan}λ%f"

  PROMPT="${(j: :)parts} "
}

# Function to handle async response
prompt_git_response() {
  typeset -g _prompt_git_fd

  prompt_git_define_prompt "$(<&$1)"
  zle reset-prompt

  zle -F $1
  exec {1}<&-
  unset _prompt_git_fd
}

# Function to run before each prompt
prompt_git_precmd() {
  typeset -g _prompt_git_fd

  prompt_git_define_prompt

  [[ -n $_prompt_git_fd ]] && {
    zle -F $_prompt_git_fd
    exec {_prompt_git_fd}<&-
  }

  exec {_prompt_git_fd}< <(prompt_git_status)
  zle -F $_prompt_git_fd prompt_git_response
}

# Add hook to run before each prompt
add-zsh-hook precmd prompt_git_precmd

# set proxy
function proxy() {
  export http_proxy=http://127.0.0.1:1087
  export https_proxy=http://127.0.0.1:1087
  export ALL_PROXY=socks5://127.0.0.1:1080
  # echo -e "\e[32mProxy has been successfully set.\e[0m"
}

# unset
function unproxy() {
  unset http_proxy
  unset https_proxy
  unset ALL_PROXY
  echo -e "\e[31mProxy has been unset.\e[0m"
}

# enable proxy
proxy

# create tmux new session with window name
tn() {
  tmux new-session -d -s $1
  tmux rename-window -t $1:1 'main'
  tmux a -t $1
}

# auto run into tmux when open shell
if [ "$TMUX" = "" ]; then tn work; fi

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

zinit ice depth=1 wait"2" lucid
zinit light hlissner/zsh-autopair

# fzf relate config
export FZF_DEFAULT_OPTS="--height 60% \
                        --border sharp \
                        --layout reverse \
                        --prompt '∷ ' \
                        --pointer ▶ \
                        --marker ⇒"

# open a file with fzf
fo() {
  IFS=$'\n' out=($(fzf --query="$1" --multi))
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-nvim} "$file"
  fi
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
