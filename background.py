import re

replace = '#002b36'

def replace_first_hex_color(filename):
    with open(filename, "r") as file:
        first_line = file.readline().strip()

        pattern = re.compile("#[0-9a-fA-F]{6}")
        match = pattern.search(first_line)

        if match:
            first_hex_color = match.group()
            first_line = first_line.replace(first_hex_color, replace)

        with open(filename + "_new", "w") as file_new:
            file_new.write(first_line + "\n")

        for line in file:
            file_new.write(line)

replace_first_hex_color("./tmux/.tmux.conf")
replace_first_hex_color("./config/kitty/current-theme.conf")
