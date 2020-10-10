# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# alias
alias vi="nvim"
alias reload="source ~/.zshrc"
alias ls="lsd"
alias ll="ls -l"
alias la="ls -la"
alias lt="ls --tree"

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
	print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
	command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
	command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
		print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
		print -P "%F{160}▓▒░ The clone has failed.%f"
fi
source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

if [ -z "$TMUX" ]
then
    tmux attach -t work || tmux new -s work
fi

zinit ice depth=1 atload"!source ~/.p10k-cloud.zsh" lucid nocd
zinit light romkatv/powerlevel10k

# Oh-my-zsh plugins
zinit snippet OMZ::lib/history.zsh

zinit snippet OMZ::lib/key-bindings.zsh

zinit ice wait lucid
zinit snippet OMZ::lib/completion.zsh

zinit ice wait lucid
zinit snippet OMZ::lib/grep.zsh

# Oh-my-zsh plugins
zinit ice wait lucid atload"unalias grv"
zinit snippet OMZ::plugins/git/git.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/systemd/systemd.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/sudo/sudo.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/extract/extract.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/systemadmin/systemadmin.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/golang/golang.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/fzf/fzf.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/autojump/autojump.plugin.zsh

# Plugins
zinit ice depth=1 lucid
zinit light trystan2k/zsh-tab-title

zinit ice depth=1 wait lucid
zinit light Aloxaf/fzf-tab

zinit ice depth=1 wait blockf lucid atpull"zinit creinstall -q ."
zinit light clarketm/zsh-completions

zinit ice depth=1 wait lucid atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay"
zinit light zdharma/fast-syntax-highlighting

zinit ice depth=1 wait lucid compile"{src/*.zsh,src/strategies/*.zsh}" atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

zinit ice depth=1 wait lucid atload"bindkey '$terminfo[kcuu1]' history-substring-search-up; bindkey '$terminfo[kcud1]' history-substring-search-down"
zinit light zsh-users/zsh-history-substring-search

zinit ice depth=1 wait lucid
zinit light wfxr/formarks

zinit ice depth=1 wait"1" lucid pick"manydots-magic" compile"manydots-magic"
zinit light knu/zsh-manydots-magic

zinit ice depth=1 wait"1" lucid atinit"zstyle ':history-search-multi-word' page-size '20'"
zinit light zdharma/history-search-multi-word

zinit ice depth=1 wait"2" lucid
zinit light wfxr/forgit

zinit ice depth=1 wait"2" lucid
zinit light hlissner/zsh-autopair

zinit ice depth=1 wait"2" lucid
zinit light peterhurford/up.zsh

zinit ice depth=1 wait"2" lucid
zinit light MichaelAquilina/zsh-you-should-use

# To customize prompt, run `p10k configure` or edit ~/.dotfiles/zsh/.p10k.zsh.
[[ ! -f ~/.dotfiles/zsh/.p10k.zsh ]] || source ~/.dotfiles/zsh/.p10k.zsh
