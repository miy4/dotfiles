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

    export PS1='%F{blue}%n%f@%F{blue}%m%f:%F{blue}%/%f$(prompt_git_current_branch)'$'\n'"%B%F{magenta}>%f%b "
}

: "Completion" && () {
    # man zshcompsys
    # http://zsh.sourceforge.net/Doc/Release/Completion-System.html
    autoload -Uz compinit && compinit -C
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

    if (( ${+commands[fzf]} )); then
        ..() {
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
            done | fzf | read togo
            if [ -n "$togo" ]; then
               cd "$togo"
            fi
        }
    fi
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
    if (( ${+commands[vivid]} )); then
        if [[ -r ~/.config/vivid/themes/challengerdeep.yml ]]; then
            export LS_COLORS=$(vivid generate challengerdeep)
        else
            export LS_COLORS=$(vivid generate snazzy)
        fi
    fi
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

    if (( ${+commands[bat]} )); then
        alias ccat='bat --plain --theme OneHalfLight'
    fi

    if (( ${+commands[prettyping]} )); then
        alias pping='prettyping --nolegend'
    fi

    if (( ${+commands[htop]} )); then
        alias ttop='htop'
    fi

    if (( ${+commands[diff-so-fancy]} )); then
        ddiff() {
            git diff "$@" \
                | diff-so-fancy \
                | less -R -f -X -i -P '?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
        }
    fi

    if (( ${+commands[ncdu]} )); then
        alias ddu='ncdu --color dark -rr -x'
    fi
}

: "Managing plugins" && () {
    source "${ZDOTDIR:-$HOME}/.zplugin/bin/zplugin.zsh"
    autoload -Uz _zplugin
    (( ${+_comps} )) && _comps[zplugin]=_zplugin

    zplugin light zsh-users/zsh-autosuggestions
    zplugin light zdharma/fast-syntax-highlighting

    zplugin ice silent wait'0' as'program' pick'bin/anyenv' atload'export ANYENV_ROOT=$PWD; eval "$(anyenv init -)"'
    zplugin light anyenv/anyenv
    zplugin ice silent as'program' pick'bin/anyenv-update'
    zplugin light znz/anyenv-update
}

: "Golang" && () {
    if (( ${+commands[go]} )); then
        export GOPATH=$HOME
    fi

    get-go-tool() {
        [[ -z $1 ]] && { echo "$0 package" 1>&2; return 1 }
        readonly package="$1"
        readonly tool_name="${package##*/}"

        GO111MODULE=on
        cat <<EOF1 > .envrc
export GO111MODULE=on
EOF1
        direnv allow

        go mod init "example.com/$package"
        cat <<EOF2 > tools.go
// +build tools
package tools
import (
	_ "$package"
)
EOF2
        go build -o $tool_name $package
    }
}

: "Git" && () {
    if [ -x /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight
    elif [ -x /usr/share/git/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/share/git/diff-highlight
    fi
}

: "Commandline Filter" && () {
    (( ${+commands[fzf]} )) || return

    export FZF_DEFAULT_OPTS="--reverse --no-sort --inline-info --multi --bind 'ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"

    select-ghq-repository() {
        local dir=$(ghq list --full-path | fzf)
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
        selected=( $(fc -rl 1 | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS -n2.. --query=${(qqq)LBUFFER} +m" fzf) )
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
}

: "Managing Environment Variables" && () {
    if (( ${+commands[direnv]} )); then
        eval "$(direnv hook zsh)"
    fi
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
