OneDark='#282c34'
NightOwl='#0F1D2A'
Material='#263238'

BACKGROUND_COLOR=$OneDark
INACTIVE_FG_COLOR='#5c6370'
ACTIVE_FG_COLOR='#fac863'

set-option -g status-style "bg=$BACKGROUND_COLOR"

# Status setup
set-option -g status on
set-option -g status-fg default
set -g status-justify left
set -g status-interval 1

# ------------------------------------------------------------------------------
# components
# ------------------------------------------------------------------------------
# NOTE: these use nested conditionals and "," and "}" must be escaped
set -g @cpu_low_icon "ᚋ"
set -g @cpu_medium_icon "ᚌ"
set -g @cpu_high_icon "ᚍ"
set -g @batt_remain_short 'true'

set -g @batt_icon_status_attached '🔌'

separator="#[fg=$INACTIVE_FG_COLOR]|#[default]"

search_icon="#{?window_zoomed_flag,#{?window_active,#[fg=blue],#[fg=default]},}"

pane_count="#{?window_active,#[fg=white#,noitalics],}"

status_items="#{?window_bell_flag,#[fg=red] ,}$search_icon $pane_count"

# see: https://github.com/tmux-plugins/tmux-battery
battery="Batt: #{battery_color_fg}#{battery_icon} #{battery_remain}#[default]"

cpu="#[bold]CPU: #[default]#{cpu_fg_color}#{cpu_icon} #{cpu_percentage}#[default]"
ram="#[bg=#282c34]RAM: #{ram_fg_color}#{ram_icon}#[default]"

# prefix
prefix="#{?client_prefix,🐠,}"

set -g status-left-length 80
# Options -> ⧉ ❐
set -g status-left "#{?client_prefix,#[fg=#ffffff bg=#22252B],#[fg=#e5c07b]} ❐ #S "
set -g status-right-length 70
set -g status-right "$prefix $cpu $separator $ram $separator $battery $separator 🕒 %a %d %b %H:%M"

set-window-option -g window-status-current-style "fg=#9ed11d"
set-window-option -g window-status-current-format "$separator #I: #[bold]#W $status_items"

# for some unknown reason this tmux section is being set to reverse from
# somewhere so we explictly remove it here
set-window-option -g window-status-style "fg=$INACTIVE_FG_COLOR dim"
set-window-option -g window-status-format "#[none] #I: #W $status_items"
set-window-option -g window-status-separator "$separator"

# Styling when in command mode i.e. vi or emacs mode in tmux command line
set -g message-command-style 'fg=green bg=default bold,blink'
# Regular tmux commandline styling
set -g message-style 'fg=yellow bg=default bold'

# Set window notifications
set-option -g monitor-activity on
set-option -g visual-activity off

