# dotfiles
my personal dotfiles mac
![imgae](https://github.com/taigacute/IMG/blob/master/dotfiles/dotfiles.png)

# tmux斜体支持
* 创建一个新的文件tmux-256color.terminfo，内容如下
```
tmux-256color|tmux with 256 colors,
  ritm=\E[23m, rmso=\E[27m, sitm=\E[3m, smso=\E[7m, Ms@,
  khome=\E[1~, kend=\E[4~,
  use=xterm-256color, use=screen-256color,
```
* 安装新终端`tic -x tmux-256color.terminfo`
* 修改.tmux.conf  --> 我的tmux已经配置了
```
set -g default-terminal 'tmux-256color'
set -as terminal-overrides ',xterm*:Tc:sitm=\E[3m'
```
