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
    if hash gls 2>/dev/null; then
        alias d='gls -G -F --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
    elif ls --version | grep -q coreutils; then
        alias d='ls -G -F --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
    else
        alias d='ls -F -G'
    fi
    alias v='d -l'
    alias da='d -a'
    alias va='v -a'

    if hash src-hilite-lesspipe.sh 2>/dev/null; then
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

    if hash tmux 2>/dev/null; then
        notify-tmux() {
            IFS=" " tmux set status-right "#[fg=colour255,bg=colour27,bold]$*#[default]"
        }
    fi
}

: "Managing plugins" && () {
    local zplug_zsh=~/.zplug/zplug
    [[ -e $zplug_zsh ]] || return

    source $zplug_zsh
    zplug "zsh-users/zsh-syntax-highlighting"
    zplug "mollifier/anyframe"
    export ENHANCD_COMMAND=ed
    export ENHANCD_FILTER=fzf:peco
    zplug "b4b4r07/enhancd", use:enhancd.sh
    export EASY_ONE_KEYBIND="^xs"
    zplug "b4b4r07/easy-oneliner", if:"which fzf"
    zplug "zsh-users/zsh-autosuggestions"
    zplug "miy4/acc7a8e9bd44c647d07c", \
          from:gist, as:command, use:describe_number
    zplug "miy4/9ad22d7270c1f1a08fed", \
          from:gist, as:command, use:tomato
    zplug "miy4/4365cc3f45a23061f36dbb3e96c2c2c6", \
          from:gist, as:command, use:date_cat
    zplug "miy4/7617a9a7336ff87df98b054cbc2776f0", \
          from:gist, as:command, use:longman, if:"which jq"
    zplug "miy4/6796014b815af1b4d01546143c5ea1be", \
          from:gist, as:command, use:wordnet, if:"which wn"

    zplug check || zplug install
    zplug load

    if zplug check "mollifier/anyframe"; then
        if hash fzf 2>/dev/null; then
            zstyle ":anyframe:selector:" use fzf
        fi
        bindkey '^xr' anyframe-widget-put-history
        bindkey '^xg' anyframe-widget-cd-ghq-repository
    fi

    if zplug check "b4b4r07/easy-oneliner"; then
        export EASY_ONE_REFFILE=~/.snippets
        export EASY_ONE_FZF_OPTS="--no-sort --reverse"
    fi

    if zplug check "zsh-users/zsh-autosuggestions"; then
        [[ $(echotc Co 2>/dev/null) == "256" ]] && export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=232'
        export ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(end-of-line vi-end-of-line)
        export ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(
            forward-char
	        forward-word
	        vi-forward-word
	        vi-forward-word-end
	        vi-forward-blank-word
	        vi-forward-blank-word-end
        )
    fi

    if zplug check "miy4/4365cc3f45a23061f36dbb3e96c2c2c6"; then
        pick_dateformat() {
            date_cat | fzf --ansi --multi \
                | awk 'BEGIN {FS="|"} {print $2}' \
                | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*#.*$//' | pbcopy
        }

        pick_date() {
            date_cat | fzf --ansi --multi \
                | awk 'BEGIN {FS="|"} {print $1}' \
                | sed 's/[[:space:]]*$//' | pbcopy
        }
    fi
}

: "Node.js" && () {
    # https://github.com/hokaccha/nodebrew
    [ -x ~/.nodebrew/current/bin/nodebrew ] || return

    export NODEBREW_ROOT=~/.nodebrew
    export PATH=$PATH:./node_modules/.bin:$NODEBREW_ROOT/current/bin
    
    node-update() {
        local prev_version=$(nodebrew ls | sed -n 's/^current: \(.*\)$/\1/p')
        printf "\033[32m%s\033[m\n" "==> nodebrew install-binary stable"
        nodebrew install-binary stable || return
        printf "\033[32m%s\033[m\n" "==> nodebrew use stable"
        nodebrew use stable
        
        [[ -z $prev_version || $prev_version == "none" ]] && return
        printf "\033[32m%s\033[m\n" "==> nodebrew migrate-package $prev_version"
        nodebrew migrate-package $prev_version
    }
}

: "Golang" && () {
    hash go 2>/dev/null || return

    export GOPATH=$HOME
    export PATH=$PATH:$GOPATH/bin:$(go env GOROOT)/bin
}

: "Rust" && () {
    export PATH=$PATH:~/.cargo/bin
    export RUST_SRC_PATH=~/src/github.com/rust-lang/rust/src
    export CARGO_HOME=~/.cargo
}

: "Java" && () {
}

: "Android" && () {
    if [ -d /opt/android-sdk ]; then
        export ANDROID_HOME=/opt/android-sdk
        export PATH=$PATH:/opt/android-sdk/tools:/opt/android-sdk/platform-tools
    elif [ -d /usr/local/opt/android-sdk ]; then
        export ANDROID_HOME=/usr/local/opt/android-sdk
    fi
}

: "Git" && () {
    if [ -x /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight
    elif [ -x /usr/share/git/diff-highlight/diff-highlight ]; then
        export PATH=$PATH:/usr/share/git/diff-highlight
    fi
}

: "Filter" && () {
    # https://github.com/junegunn/fzf
    if hash fzf 2>/dev/null; then
        export FZF_DEFAULT_OPTS="--reverse --no-sort --inline-info --multi --bind 'ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"
    fi
}

: "Managing Environment Variables" && () {
    # https://github.com/direnv/direnv
    if hash direnv 2>/dev/null; then
        eval "$(direnv hook zsh)"
    fi
}

: "Loading a site local rc file" && () {
    local zshrc_site=~/.config/zsh/.zshrc.site
    [[ -f $zshrc_site ]] && source "$zshrc_site"
}

: "Profiling for zsh startup" && () {
    if type zprof >/dev/null 2>&1; then
        # `zmodload zsh/zprof && zprof` in advance
        zprof | less
    fi
}
