source $(dirname $(gem which colorls))/tab_complete.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias f="ranger"
# alias v="vifm"
alias vi="nvim"
alias vim="vim"
alias lc='colorls -lA --sd'
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs "
alias reload="source ~/.zshrc"
# alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
# alias emacs="/usr/local/Cellar/emacs-mac/emacs-26.2-z-mac-7.6/Emacs.app"


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
zplug 'plugins/git', from:oh-my-zsh, if:'which git'
zplug "plugins/autojump", from:oh-my-zsh, defer:2
zplug 'romkatv/powerlevel10k', use:powerlevel10k.zsh-theme
zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions', defer:2
zplug 'zsh-users/zsh-history-substring-search'
zplug 'zsh-users/zsh-syntax-highlighting', defer:2

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

POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

# LEFT_PROMPT
# ===========================================================
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon battery ram time newline dir_joined)
POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR=''
POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR=' '
POWERLEVEL9K_WHITESPACE_BETWEEN_LEFT_SEGMENTS=''

# status
# POWERLEVEL9K_STATUS_VERBOSE=false

# os_icon custom
POWERLEVEL9K_OS_ICON_BACKGROUND='none'
POWERLEVEL9K_OS_ICON_FOREGROUND='004'

# battery
POWERLEVEL9K_BATTERY_LOW_BACKGROUND='none'
POWERLEVEL9K_BATTERY_LOW_FOREGROUND='001'
POWERLEVEL9K_BATTERY_CHARGING_BACKGROUND='none'
POWERLEVEL9K_BATTERY_CHARGING_FOREGROUND='076'
POWERLEVEL9K_BATTERY_CHARGED_BACKGROUND='none'
POWERLEVEL9K_BATTERY_CHARGED_FOREGROUND='076'
POWERLEVEL9K_BATTERY_DISCONNECTED_BACKGROUND='none'
POWERLEVEL9K_BATTERY_DISCONNECTED_FOREGROUND='003'
POWERLEVEL9K_BATTERY_LOW_THRESHOLD=15
POWERLEVEL9K_BATTERY_VERBOSE=false
POWERLEVEL9K_BATTERY_STAGES=''

# time
# POWERLEVEL9K_TIME_FORMAT="%D{%d.%m.%y %H:%M:%S}"
POWERLEVEL9K_TIME_ICON=''
POWERLEVEL9K_TIME_BACKGROUND='none'
POWERLEVEL9K_TIME_FOREGROUND='006'

# ram
POWERLEVEL9K_RAM_ICON=''
POWERLEVEL9K_RAM_BACKGROUND='none'
POWERLEVEL9K_RAM_FOREGROUND='006'

# dir
POWERLEVEL9K_SHORTEN_DELIMITER=''
POWERLEVEL9K_SHORTEN_DIR_LENGTH=7
# POWERLEVEL9K_SHORTEN_STRATEGY='truncate_to_first_and_last'

POWERLEVEL9K_ETC_ICON=''
POWERLEVEL9K_FOLDER_ICON=''
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''

POWERLEVEL9K_DIR_ETC_BACKGROUND='none'
POWERLEVEL9K_DIR_ETC_FOREGROUND='005'
POWERLEVEL9K_DIR_HOME_BACKGROUND='none'
POWERLEVEL9K_DIR_HOME_FOREGROUND='004'
POWERLEVEL9K_DIR_DEFAULT_BACKGROUND='none'
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND='005'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_BACKGROUND='none'
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND='004'

# customization
# POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR=''

# RIGHT_PROMP
# ===========================================================
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(vcs)
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR=''
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR=' '
POWERLEVEL9K_WHITESPACE_BETWEEN_RIGHT_SEGMENTS=''

# vcs
POWERLEVEL9K_SHOW_CHANGESET=true
POWERLEVEL9K_CHANGESET_HASH_LENGTH=6

POWERLEVEL9K_VCS_CLEAN_BACKGROUND='none'
POWERLEVEL9K_VCS_CLEAN_FOREGROUND='076'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='none'
POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND='005'
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='none'
POWERLEVEL9K_VCS_MODIFIED_FOREGROUND='003'

POWERLEVEL9K_VCS_GIT_HOOKS=(vcs-detect-changes git-untracked git-aheadbehind git-remotebranch git-tagname)

POWERLEVEL9K_MODE='nerdfont-complete'
