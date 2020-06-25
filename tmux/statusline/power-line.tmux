set -g status 'on'
set -g status-position top
set -g status-justify 'centre'
set -g status-left-length '100'
set -g status-right-length '100'
setw -g window-status-separator ''

set -g status-left '#[fg=colour232,bg=colour154] #(whoami) \
#[fg=colour154,bg=colour238,nobold,nounderscore,noitalics]\
#[fg=colour222,bg=colour238] #W \
#[fg=colour238,bg=default,nobold,nounderscore,noitalics]'

set -g status-right '\
#[fg=colour238,bg=default,nobold,nounderscore,noitalics]\
#[fg=colour222,bg=colour238] %r \
#[fg=colour154,bg=colour238,nobold,nounderscore,noitalics]\
#[fg=colour232,bg=colour154] %a-%Y'

setw -g window-status-format '\
#[fg=colour235,bg=colour235,nobold,nounderscore,noitalics]\
#[default] #I  #W #[fg=colour235,bg=colour235,nobold,nounderscore,noitalics]'

setw -g window-status-current-format '\
#[fg=colour235,bg=colour238,nobold,nounderscore,noitalics]\
#[fg=colour222,bg=colour238] #I  #W  #F \
#[fg=colour238,bg=colour235,nobold,nounderscore,noitalics]'
