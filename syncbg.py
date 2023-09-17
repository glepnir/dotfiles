import os

def update_tmux_conf(hexcode):
    tmux_conf_path = os.path.expanduser("~/.dotfiles/config/tmux/tmux.conf")

    with open(tmux_conf_path, "r") as f:
        lines = f.readlines()

    with open(tmux_conf_path, "w") as f:
        for line in lines:
            if line.strip().startswith("set -g background"):
                f.write(f"set -g background {hexcode}\n")
            else:
                f.write(line)

def update_kitty_conf(hexcode):
    kitty_conf_path = os.path.expanduser("~/.dotfiles/config/kitty/current-theme.conf")

    with open(kitty_conf_path, "r") as f:
        lines = f.readlines()

    with open(kitty_conf_path, "w") as f:
        for line in lines:
            if line.strip().startswith("background "):
                f.write(f"background      {hexcode}\n")
            else:
                f.write(line)

if __name__ == "__main__":
    hexcode = input("new background hexcode：")
    
    update_tmux_conf(hexcode)
    update_kitty_conf(hexcode)

    print("config file updated！")
