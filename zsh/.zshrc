source $(dirname $(gem which colorls))/tab_complete.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load dotfiles
for file in $dotfiles/zsh/.{alias}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file";
done;

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# zplug configruation
if [[ ! -d "${ZPLUG_HOME}" ]]; then
  if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    # If we can't get zplug, it'll be a very sobering shell experience. To at
    # least complete the sourcing of this file, we'll define an always-false
    # returning zplug function.
    if [[ $? != 0 ]]; then
      function zplug() {
        return 1
      }
    fi
  fi
  export ZPLUG_HOME=~/.zplug
fi
if [[ -d "${ZPLUG_HOME}" ]]; then
  source "${ZPLUG_HOME}/init.zsh"
fi
zplug "plugins/git",               from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/sudo",              from:oh-my-zsh, if:"(( $+commands[sudo] ))"
zplug "plugins/golang",            from:oh-my-zsh, if:"(( $+commands[go] ))"
zplug "plugins/node",              from:oh-my-zsh, if:"(( $+commands[node] ))"
zplug "plugins/npm",               from:oh-my-zsh, if:"(( $+commands[npm] ))"
zplug "plugins/docker",            from:oh-my-zsh, if:"(( $+commands[docker] ))"
zplug "plugins/docker-compose",    from:oh-my-zsh, if:"(( $+commands[docker-compose] ))"

zplug "plugins/autojump", from:oh-my-zsh, defer:2
zplug 'romkatv/powerlevel10k', use:powerlevel10k.zsh-theme
zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions', defer:2
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-history-substring-search', defer:3

if ! zplug check; then
  zplug install
fi

zplug load

if zplug check 'zsh-users/zsh-autosuggestions'; then
  # Enable asynchronous fetching of suggestions.
  ZSH_AUTOSUGGEST_USE_ASYNC=1
  # For some reason, the offered completion winds up having the same color as
  # the terminal background color (when using a dark profile). Therefore, we
  # switch to gray.
  # See https://github.com/zsh-users/zsh-autosuggestions/issues/182.
  ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
fi

# zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]='fg=blue'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[function]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=cyan,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=magenta,bold'
ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=yellow,bold'

# zsh-theme
#DISABLE_AUTO_TITLE="true"

POWERLEVEL9K_BATTERY_CHARGING='yellow'
POWERLEVEL9K_BATTERY_CHARGED='green'
POWERLEVEL9K_BATTERY_DISCONNECTED='$DEFAULT_COLOR'
POWERLEVEL9K_BATTERY_LOW_THRESHOLD='10'
POWERLEVEL9K_BATTERY_LOW_COLOR='red'
POWERLEVEL9K_BATTERY_ICON='\uf1e6 '
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=''
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX='\uf0da'
#POWERLEVEL9K_VCS_GIT_ICON='\ue60a'

POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='yellow'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='yellow'
#POWERLEVEL9K_VCS_UNTRACKED_ICON='?'


POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status os_icon battery context dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(time background_jobs ram virtualenv rbenv rvm)

POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=4

#POWERLEVEL9K_CUSTOM_TIME_FORMAT="%D{\uf017 %H:%M:%S}"
POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073 %d.%m.%y}"

POWERLEVEL9K_STATUS_VERBOSE=false

POWERLEVEL9K_PROMPT_ON_NEWLINE=true
