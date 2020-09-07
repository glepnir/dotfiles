# Move status bar to the top
set -g status 'on'
set -g status-position top
set -g status-justify 'centre'
set -g status-left-length '100'
set -g status-right-length '100'

set-window-option -g window-status-format '#[bg=colour238]#[fg=colour107] #I #[bg=colour239]#[fg=colour110] #[bg=colour240]#W#[bg=colour239]#[fg=colour195]#F#[bg=colour238] '
set-window-option -g window-status-current-format '#[bg=colour236]#[fg=colour215,bold] #I:#[bg=colour235]#[fg=colour167] #[bg=colour234]#W#[bg=colour235]#[fg=colour195]#F#[bg=colour236] '

set -g status-left '\
#[fg=colour232,bg=#6272a4] %Y-%m-%d \
#[bg=#1b2b34] \
#[fg=colour232,bg=#6272a4] %a %H:%M '
#[fg=colour232,bg=colour154] #(rainbarf --battery --remaining --no-rgb) '

set -g status-right '\
#{?client_prefix,🐠,} \
#[fg=colour232,bg=#6272a4] CPU:#{cpu_percentage} \
#[bg=#1b2b34] \
#[fg=colour232,bg=#6272a4] MEM:#{ram_percentage} '
