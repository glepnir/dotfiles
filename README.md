# dotfiles

my personal dotfiles mac
![imgae](https://github.com/taigacute/dotfiles/blob/master/img/desktop.png)

# tmux 斜体支持

- 创建一个新的文件 tmux-256color.terminfo，内容如下

```
tmux-256color|tmux with 256 colors,
  ritm=\E[23m, rmso=\E[27m, sitm=\E[3m, smso=\E[7m, Ms@,
  khome=\E[1~, kend=\E[4~,
  use=xterm-256color, use=screen-256color,
```

- 安装新终端`tic -x tmux-256color.terminfo`
- 修改.tmux.conf --> 我的 tmux 已经配置了

```
set -g default-terminal 'tmux-256color'
set -as terminal-overrides ',xterm*:Tc:sitm=\E[3m'
```

# Install Tmux

安装 tmux 插件管理 tpm

```
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

安装 tmux 插件 `prefix` + I
