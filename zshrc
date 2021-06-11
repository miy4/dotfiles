# -*- mode: sh -*-

: "History" && () {
    # man zshparam
    # http://zsh.sourceforge.net/Doc/Release/Parameters.html#Parameters-Used-By-The-Shell
    export HISTFILE=~/.local/share/zsh/history
    export HISTSIZE=100000
    export SAVEHIST=100000

    # man zshoptions
    # http://zsh.sourceforge.net/Doc/Release/Options.html#History
    setopt EXTENDED_HISTORY
    setopt HIST_IGNORE_DUPS
    setopt HIST_IGNORE_SPACE
    setopt HIST_REDUCE_BLANKS
    setopt HIST_VERIFY
    setopt SHARE_HISTORY
}

: "Line Editing" && () {
    # man zshoptions, zshzle
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Zle
    # http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
    setopt EMACS
    unsetopt BEEP

    # treat non-alphanumeric chars as word delimiters
    autoload -Uz select-word-style
    select-word-style default
    zstyle ':zle:*' word-chars " *+?_-.[]~=&;:!#$%^(){}<>/@|"
    zstyle ':zle:*' word-style unspecified
}

: "Input/Output" && () {
    # man zshoptions
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Input_002fOutput
    setopt CORRECT
    setopt PRINT_EIGHT_BIT
}

: "Prompting" && () {
    # man zshoptions, zshmisc
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Prompting
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Prompt-Expansion
    autoload -Uz vcs_info
    zstyle ':vcs_info:*' enable git svn
    zstyle ':vcs_info:*' max-exports 6
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' formats '%b@%r' '%c' '%u'
    zstyle ':vcs_info:git:*' actionformats '%b@%r|%a' '%c' '%u'

    setopt PROMPT_SUBST

    prompt_git_current_branch() {
        [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == "true" ]] || return

        STY= LANG=en_US.UTF-8 vcs_info
        local color
        if   [[ -n $vcs_info_msg_1_ ]]; then color="green"   # staged
        elif [[ -n $vcs_info_msg_2_ ]]; then color="red"     # unstaged
        elif git status 2>/dev/null | grep -q "^Untracked"; then color="cyan"  # untracked
        else color="blue"
        fi
        local branch="$vcs_info_msg_0_"
        echo " (%F{$color}$branch%f)"
    }

    export PS1='%F{blue}%n%f@%F{blue}%m%f:%F{blue}%/%f$(prompt_git_current_branch)'$'\n'"%B%F{magenta}❱❱%f%b "
}

: "Completion" && () {
    # man zshcompsys
    # http://zsh.sourceforge.net/Doc/Release/Completion-System.html
    autoload -Uz compinit && compinit -u
    local zsh_completions="/usr/local/share/zsh-completions"
    [[ -d $zsh_completions ]] && fpath=($zsh_completions $fpath)

    # man zshoptions
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Completion-2
    setopt AUTO_LIST
    setopt AUTO_MENU
    zstyle ':completion:*:default' menu select=1
    [[ -n $LS_COLORS ]] && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

    setopt LIST_PACKED
    setopt LIST_TYPES
    setopt AUTO_PARAM_KEYS
    setopt AUTO_PARAM_SLASH
}

: "Changing Directories" && () {
    # man zshoptions
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Changing-Directories
    setopt AUTO_CD

    ..() {
        if (( ${+commands[sk]} )); then
            filter='sk'
        elif (( ${+commands[fzf]} )); then
            filter='fzf'
        else
            printf "no filter command available\n"
            return
        fi

        local dir="$PWD"
        local togo
        while true; do
            dir=${dir%/*}
            if ((${#dir})); then
                echo $dir
            else
                echo '/'
                break
            fi
        done | $filter | read togo
        if [ -n "$togo" ]; then
            cd "$togo"
        fi
    }
}

: "Expansion and Globbing" && () {
    # man zshoptions
    # http://zsh.sourceforge.net/Doc/Release/Options.html#Expansion-and-Globbing
    setopt EXTENDED_GLOB
    setopt MAGIC_EQUAL_SUBST
    setopt NUMERIC_GLOB_SORT
    setopt REMATCH_PCRE
}

: "Color and Theme" && () {
    #if (( ${+commands[vivid]} )); then
    #    if [[ -r ~/.config/vivid/themes/challengerdeep.yml ]]; then
    #        export LS_COLORS=$(vivid generate challengerdeep)
    #    else
    #        export LS_COLORS=$(vivid generate snazzy)
    #    fi
    #fi
}

: "Aliasing" && () {
    # man zshmisc
    # http://zsh.sourceforge.net/Doc/Release/Shell-Grammar.html#Aliasing
    if (( ${+commands[gls]} )); then
        alias d='gls -G -F --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
    elif ls --version | grep -q coreutils; then
        alias d='ls -G -F --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
    else
        alias d='ls -F -G'
    fi
    alias v='d -l'
    alias da='d -a'
    alias va='v -a'

    if (( ${+commands[src-hilite-lesspipe.sh]} )); then
        alias l="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRi"
        alias lF="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRij10 +F"
    else
        alias l='less -sNRi'
        alias lF='less -sNRij10 +F'
    fi

    alias g='grep --color=auto'
    alias psv='ps auxww'
    alias reload='exec zsh -l'
    alias ec='emacsclient -n'

    function = {
        bc -l <<< "$@"
    }

    if [[ $(uname) == "Darwin" ]]; then
        alias suspend='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'
    fi

    if (( ${+commands[tmux]} )); then
        notify-tmux() {
            IFS=" " tmux set status-right "#[fg=colour255,bg=colour27,bold]$*#[default]"
        }
    fi

    (( ${+commands[bat]} )) && alias ccat='bat --plain --theme OneHalfLight'
    (( ${+commands[prettyping]} )) && alias pping='prettyping --nolegend'
    (( ${+commands[htop]} )) && alias ttop='htop'
    (( ${+commands[ncdu]} )) && alias ddu='ncdu --color dark -rr -x'

    if (( ${+commands[icdiff]} )); then
        alias ddiff=icdiff
    elif (( ${+commands[diff-so-fancy]} )); then
        ddiff() {
            git diff "$@" \
                | diff-so-fancy \
                | less -R -f -X -i -P '?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
        }
    fi

    http-server() {
        local -r http_port=${1:-8080}
        if (( ${+commands[python]} )); then
            if python --version | grep -q '^Python 3\.'; then
                python -m http.server $http_port
            else
                python -m SimpleHTTPServer $http_port
            fi
        elif (( ${+commands[python3]} )); then
            python3 -m http.server $http_port
        elif (( ${+commands[python2]} )); then
            python2 -m SimpleHTTPServer $http_port
        elif (( ${+commands[ruby]} )); then
            ruby -run -e httpd . -p $http_port
            #ruby -rwebrick -e 'WEBrick::HTTPServer.new(:DocumentRoot => "./", :Port => 8080).start'
        elif (( ${+commands[php]} )); then
            php -S localhost:$http_port
        else
            printf "nothing to do :(\n" 1>&2
        fi
    }

    new() {
        local dir file
        while [[ -n $1 ]]; do
            dir=$(dirname "$1")
            file="$1"

            if [[ ! -d $dir ]]; then
                mkdir --parents "$dir"
            fi

            if [[ -f $file ]]; then
                printf "You already have done it: %s\n" "$file" 1>&2
            else
                touch "$file"
            fi
            shift
        done
    }

    gist-view() {
        gh gist view $(gh gist list | sk | awk '{print $1}')
    }
}

: "Pager and Manual" && () {
    export PAGER=less
    export MANPAGER='less -X'
    export LESSCHARSET=utf-8
    export LESS='-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'

    man() {
        LESS_TERMCAP_mb=$(printf "\e[1m") \
        LESS_TERMCAP_md=$(printf "\e[1;34m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;33m") \
        LESS_TERMCAP_ue=$(printf "\e[24;0m") \
        LESS_TERMCAP_us=$(printf "\e[4;32m") \
        command man "$@"
    }

    if (( ${+commands[pandoc]} )); then
        md() {
            LESS_TERMCAP_mb=$(printf "\e[1m") \
            LESS_TERMCAP_md=$(printf "\e[1;34m") \
            LESS_TERMCAP_me=$(printf "\e[0m") \
            LESS_TERMCAP_se=$(printf "\e[0m") \
            LESS_TERMCAP_so=$(printf "\e[1;33m") \
            LESS_TERMCAP_ue=$(printf "\e[24;0m") \
            LESS_TERMCAP_us=$(printf "\e[4;32m") \
            pandoc -s -f markdown -t man "$1" | groff -t -T utf8 -man | sed 1,4d | less
        }
    fi
}

: "Managing plugins" && () {
    local -r zas="${ZDOTDIR:-$HOME}/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
    if [[ -f $zas ]]; then
        source $zas
    fi

    local -r fsh="${ZDOTDIR:-$HOME}/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
    if [[ -f $fsh ]]; then
        source $fsh
    fi
}

: "Managing Environment Variables" && () {
    if (( ${+commands[direnv]} )); then
        eval "$(direnv hook zsh)"
    fi
}

: "Golang" && () {
    if (( ${+commands[go]} )); then
        export GOPATH=${HOME}/opt/go
        if [[ -d ${GOPATH}/bin ]]; then
            export PATH=${GOPATH}/bin:$PATH
        fi

        export GO111MODULE=on
    fi
}

: "Javascript" && () {
    if [[ -r ~/.config/npm/npmrc ]]; then
        export NPM_CONFIG_USERCONFIG=~/.config/npm/npmrc
    fi
}

: "Git" && () {
    if [ -x /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight
    elif [ -x /usr/share/git/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/share/git/diff-highlight
    fi
}

: "AWS" && () {
    export AWS_SHARED_CREDENTIALS_FILE=~/.config/aws/credentials
    export AWS_CONFIG_FILE=~/.config/aws/config
}

: "Commandline Filter" && () {
    if (( ${+commands[sk]} )); then
        export SKIM_DEFAULT_OPTIONS="--regex --reverse --multi --bind 'ctrl-j:ignore,ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"
    elif (( ${+commands[fzf]} )); then
        export FZF_DEFAULT_OPTS="--reverse --no-sort --inline-info --multi --bind 'ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"
    else
        return
    fi

    select-ghq-repository() {
        local dir
        if (( ${+commands[sk]} )); then
            dir=$(ghq list --full-path | sk)
        elif (( ${+commands[fzf]} )); then
            dir=$(ghq list --full-path | fzf)
        else
            printf "no filter command available\n"
            return
        fi
	    if [[ -n "$dir" ]]; then
	        cd "$dir"
	        if zle; then
                zle reset-prompt
            fi
	    fi
    }
    zle -N select-ghq-repository
    bindkey '^xg' select-ghq-repository

    select-history() {
        local selected num
        if (( ${+commands[sk]} )); then
            selected=( $(fc -rl 1 | sk -n 2.. --query="${LBUFFER}" --no-multi) )
        elif (( ${+commands[fzf]} )); then
            selected=( $(fc -rl 1 | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS -n2.. --query=${(qqq)LBUFFER} +m" fzf) )
        else
            printf "no filter command available\n"
            return
        fi
        local ret=$?
        if [ -n "$selected" ]; then
            num=$selected[1]
            if [ -n "$num" ]; then
                zle vi-fetch-history -n $num
            fi
        fi
        zle reset-prompt
	    return $ret
    }
    zle -N select-history
    bindkey '^xr' select-history
    
    select-command-snippet() {
        local snippets=~/.local/share/cmdcat/bundle.my.cmdcat
        local cmd=$(sk --read0 --preview 'if ((${+commands[pygmentize]})); then; echo {} | pygmentize -f terminal16m -O style=manni -l shell; else; echo {}; fi' --preview-window down:80% < "$snippets" | sed '/^#/d')
        if [[ -z $cmd ]]; then
            return 1
        fi

        lbuffer_size=${#LBUFFER}
        RBUFFER="$cmd"
        CURSOR=$(($lbuffer_size + ${#cmd}))
        zle redisplay
    }
    zle -N select-command-snippet
    bindkey '^xs' select-command-snippet

    pick-gist-id() {
        local -r gist_id=$(gh gist list | sk | awk '{print $1}')
        local -r lbuffer_size=${#LBUFFER}
        RBUFFER="$gist_id"
        CURSOR=$(($lbuffer_size + ${#gist_id}))
        zle redisplay
    }
    zle -N pick-gist-id
    bindkey '^xp' pick-gist-id
}

: "Configuring functionalities in WSL" && () {
    [[ -d /mnt/c/Windows/System32 ]] || return

    alias open='/mnt/c/Program\ Files/PowerShell/7/pwsh.exe /c start'
	alias pbcopy='/mnt/c/Windows/System32/clip.exe'
	alias pbpaste='/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe Get-Clipboard'
    export BROWSER='/mnt/c/Program\ Files/PowerShell/7/pwsh.exe /c start'

    launchx() {
        # https://gist.github.com/ctaggart/68ead4d0d942b240061086f4ba587f5f
        /mnt/c/Program\ Files/VcXsrv/vcxsrv.exe :0 \
            -multiwindow -clipboard -primary -wgl -ac > /dev/null 2>&1 &
        export DISPLAY=$(awk '/nameserver/ {print $2; exit;}' /etc/resolv.conf):0.0
        export LIBGL_ALWAYS_INDIRECT=1
    }
}

: "Loading a site local rc file" && () {
    local zshrc_site=~/.config/zsh/.zshrc.site
    [[ -f $zshrc_site ]] && source "$zshrc_site"
}

: "Profiling for zsh startup" && () {
    if (( ${+commands[zprof]} )); then
        # `zmodload zsh/zprof && zprof` in advance
        zprof | less
    fi
}
