# -*- mode: sh -*-

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=''

export EDITOR='emacsclient'

export PAGER=less
export MANPAGER='less -X'
export LESSCHARSET=utf-8
export LESS='-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/bin:/usr/sbin:/sbin:/usr/X11/bin:~/bin
if [ -d /usr/local/opt/coreutils/libexec/gnubin ]; then
    # use coreutils instead of BSD
    PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
fi
export PATH

if [ -r ~/ZPROF_ENABLE ]; then
    zmodload zsh/zprof && zprof
fi
