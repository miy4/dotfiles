# -*- mode: sh -*-

# Ensure that /etc/zsh/zshenv contains the following lines:
#
# export ZDOTDIR=${HOME}/.config/zsh
# export HISTFILE=${HOME}/.local/share/zsh/history

ulimit -c unlimited

if [[ $(umask) = "0000" ]]; then
    # WSL does not apply default umask
    umask 022
fi

export SHELL=/usr/bin/zsh
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export LANG=ja_JP.UTF-8
export EDITOR=vim
export LESSHISTFILE=-

if [[ $(uname) = "Darwin" ]]; then
    export HOMEBREW_NO_ANALYTICS=1
fi

PATH=~/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/X11/bin
if [ -d /usr/X11/bin ]; then
    PATH=$PATH:/usr/X11/bin
fi
if [ -d /usr/local/opt/coreutils/libexec/gnubin ]; then
    # use coreutils instead of BSD
    PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
fi
if [ -d ~/.linuxbrew ]; then
    PATH=~/.linuxbrew/bin:~/.linuxbrew/sbin:$PATH
elif [ -d /home/linuxbrew/.linuxbrew ]; then
    PATH=/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH
fi
export PATH

if [[ -d ${HOME}/opt/cargo ]]; then
    export CARGO_HOME=${HOME}/opt/cargo
    export PATH=${CARGO_HOME}/bin:$PATH
fi

if [[ -d ${HOME}/opt/rustup ]]; then
    export RUSTUP_HOME=${HOME}/opt/rustup
fi

if [[ -d ${HOME}/.local/share/gnupg ]]; then
    export GNUPGHOME=${HOME}/.local/share/gnupg
fi

if (( ${+commands[timew]} )); then
    export TIMEWARRIORDB=${HOME}/.config/timewarrior
fi

if [ -r ~/ZPROF_ENABLE ]; then
    zmodload zsh/zprof && zprof
fi
