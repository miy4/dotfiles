# -*- mode: sh -*-

__has_file() { [[ -f $1 ]] }
__has_dir() { [[ -d $1 ]] }
__has_executable() { [[ -x $1 ]] }
__command_found() { type "$1" >/dev/null 2>&1 }
__empty() { [[ -z $1 ]] }
__not_null() { [[ -n $1 ]] }
__on_osx() { [[ $(uname) == "Darwin" ]] }
__on_linux() { [[ $(uname) == "Linux" ]] }
__echo_colored() {
    local color=$1
    shift
    printf "\033[${color}m%s\033[m\n" "$@"
}
__echo_red() { __echo_colored 31 "$@"; }
__echo_green() { __echo_colored 32 "$@"; }
__echo_yellow() { __echo_colored 33 "$@"; }
__echo_blue() { __echo_colored 34 "$@"; }
__echo_magenta() { __echo_colored 35 "$@"; }
__echo_cyan() { __echo_colored 36 "$@"; }

my-zsh::history() {
    export HISTFILE=~/.zsh-history
    export HISTSIZE=100000
    export SAVEHIST=100000

    # コマンドの開始時刻、実行時間もヒストリファイルに書き込む
    setopt extended_history

    # 直前と同じコマンドをヒストリに追加しない
    setopt hist_ignore_dups

    # 先頭にスペースをいれると、コマンドはヒストリに追加しない
    setopt hist_ignorespace

    # 余分なスペースを除いてからヒストリに追加する
    setopt hist_reduce_blanks

    # ヒストリ展開時、いきなりコマンド実行せずに一旦提示する
    setopt hist_verify

    # 稼働するzshプロセスでヒストリを共有
    setopt share_history
}

my-zsh::command_line_editting() {
    # Emacs風のキーバインド
    bindkey -e

    # Alt+F, Alt+B でアルファベット、マルチバイト文字以外の文字を区切り文字とする
    autoload -Uz select-word-style
    select-word-style default
    zstyle ':zle:*' word-chars " /:@+|"
    zstyle ':zle:*' word-style unspecified

    # Ctrl-sでサスペンドを抑止
    stty stop undef

    # ビープを鳴らさない
    setopt nobeep

    # 出力時8ビットを通す
    setopt print_eight_bit

    # コマンドを打ち間違えた時に修正を提案する
    setopt correct

    # プロンプト
    autoload -Uz vcs_info
    zstyle ':vcs_info:*' enable git svn
    zstyle ':vcs_info:*' max-exports 6
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' formats '%b@%r' '%c' '%u'
    zstyle ':vcs_info:git:*' actionformats '%b@%r|%a' '%c' '%u'
    setopt prompt_subst
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
    PROMPT='%F{blue}%n%f@%F{blue}%m%f:%F{blue}%/%f$(prompt_git_current_branch)'$'\n'"%F{magenta}❯%f "
}

my-zsh::completion() {
    autoload -Uz compinit && compinit -C
    zsh_completions=/usr/local/share/zsh-completions
    __has_dir $zsh_completions && fpath=($zsh_completions $fpath)

    # 補完候補を一覧表示
    setopt auto_list

    # 補完候補が複数ある時、補完キー連打でメニュー選択するよう切り替わる
    setopt auto_menu
    zstyle ':completion:*:default' menu select=1
    __not_null $LS_COLORS && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

    # 一覧の行数をなるべく少なくする
    setopt list_packed

    # 補完候補一覧でファイルの種別をマーク表示
    setopt list_types

    # カッコの対応などを自動的に補完
    setopt auto_param_keys

    # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
    setopt auto_param_slash

    # =command を command のパス名に展開する
    setopt equals

    # --prefix=/usr などの = 以降も補完
    setopt magic_equal_subst

    # ファイル名の展開で辞書順ではなく数値的にソート
    setopt numeric_glob_sort
}

my-zsh::dir() {
    # 同じディレクトリを pushd しない
    setopt pushd_ignore_dups

    # ディレクトリ名だけで cd
    setopt auto_cd

    # ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
    setopt extended_glob
}

my-zsh::alias() {
    if hash gls >/dev/null 2>&1; then
        alias d='gls -G -F --color=auto --group-directories-first'
    elif ls --version | grep -q coreutils; then
        alias d='ls -G -F --color=auto --group-directories-first'
    else
        alias d='ls -F -G'
    fi
    alias v='d -l'
    alias da='d -a'
    alias va='v -a'

    if __command_found src-hilite-lesspipe.sh; then
        alias l="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRi"
        alias lF="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRij10 +F"
    else
        alias l='less -sNRi'
        alias lF='less -sNRij10 +F'
    fi

    alias g='grep --color=auto'

    alias psv='ps auxww'

    alias reload='exec zsh -l'

    autoload -Uz zmv
    alias rename='noglob zmv -W'

    alias ec='emacsclient -n'

    alias gho='dir=$(ghq list --full-path | peco --query "$LBUFFER") && [[ -n "$dir" ]] && gh-open $dir'

    if __has_dir ~/Dropbox/books; then
        alias dropbook='open "$(ls -1d ~/Dropbox/books/* | peco)"'
    fi

    function = {
        bc -l <<< "$@"
    }

    if __on_linux; then
        if [[ $(lsb_release -is 2>/dev/null) == "Arch" ]]; then
            alias update="sudo -E pacman -Syu"
        fi
    fi

    if __on_osx; then
        alias suspend='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

        update() {
            __echo_green "==> Software Update Tool"
            sudo softwareupdate -i -a

            if __command_found mas; then
                __echo_green "==> Mac App Store Update"
                mas upgrade
            fi

            if __command_found brew; then
                __echo_green "==> Homebrew"
                brew update && brew upgrade
                [[ $? -eq 0 ]] || return

                brew cask >/dev/null 2>&1 || return
                __echo_green "==> Homebrew Cask"
                for c in $(brew cask list); do
                    ! brew cask info $c | grep -qF "Not installed" || brew cask install $c;
                done
                [[ $? -eq 0 ]] || return

                for c in /opt/homebrew-cask/Caskroom/*; do
                    versions=($(ls -t $c)) && for v in "${versions[@]:1}"; do \rm -rf "$c/$v"; done;
                done
            fi
        }
    fi

    if __command_found tmux; then
        notify-tmux() {
            IFS=" " tmux set status-right "#[fg=colour255,bg=colour27,bold]$*#[default]"
        }
    fi
}

my-zsh::zplug() {
    local zplug_zsh=~/src/github.com/b4b4r07/zplug/zplug
    __has_file $zplug_zsh || return

    source $zplug_zsh
    zplug "zsh-users/zsh-syntax-highlighting"
    zplug "mollifier/anyframe"
    export ENHANCD_COMMAND=ed
    export ENHANCD_FILTER=fzf:peco
    zplug "b4b4r07/enhancd", of:enhancd.sh
    export EASY_ONE_REFFILE=~/.snippets
    export EASY_ONE_KEYBIND="^xs"
    zplug "b4b4r07/easy-oneliner", of:easy-oneliner.zsh, if:"which fzf"
    zplug "zsh-users/zsh-autosuggestions"

    zplug check || zplug install
    zplug load

    if zplug check "zsh-users/zsh-history-substring-search"; then
        bindkey -M emacs '^P' history-substring-search-up
        bindkey -M emacs '^N' history-substring-search-down
    fi

    if zplug check "mollifier/anyframe"; then
        if __command_found fzf; then
            zstyle ":anyframe:selector:" use fzf
        fi
        bindkey '^xr' anyframe-widget-put-history
        bindkey '^xg' anyframe-widget-cd-ghq-repository
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
}

my-zsh::javascript() {
    if __has_executable ~/.nodebrew/current/bin/nodebrew; then
        export NODEBREW_ROOT=~/.nodebrew
        export PATH=$PATH:./node_modules/.bin:$NODEBREW_ROOT/current/bin

        node-update() {
            local prev_version=$(nodebrew ls | sed -n 's/^current: \(.*\)$/\1/p')
            __echo_green "==> nodebrew install-binary stable"
            nodebrew install-binary stable || return
            __echo_green "==> nodebrew use stable"
            nodebrew use stable

            [[ -z $prev_version || $prev_version == "none" ]] && return
            __echo_green "==> nodebrew migrate-package $prev_version"
            nodebrew migrate-package $prev_version
        }
    fi
}

my-zsh::golang() {
    if __command_found go; then
        export GOPATH=$HOME
        export PATH=$PATH:$GOPATH/bin:$(go env GOROOT)/bin
    fi
}

my-zsh::java() {
    local java_home_cmd=/usr/libexec/java_home
    if __has_executable $java_home_cmd; then
        export JAVA_HOME=$($java_home_cmd -v 1.8)
    fi
}

my-zsh::android() {
    if __has_dir /opt/android-sdk; then
        export ANDROID_HOME=/opt/android-sdk
        export PATH=$PATH:/opt/android-sdk/tools:/opt/android-sdk/platform-tools
    elif __has_dir /usr/local/opt/android-sdk; then
        export ANDROID_HOME=/usr/local/opt/android-sdk
    fi
}

my-zsh::git() {
    if __has_executable /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight; then
        export PATH=$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight
    elif __has_executable /usr/share/git/diff-highlight/diff-highlight; then
        export PATH=$PATH:/usr/share/git/diff-highlight
    fi
}

my-zsh::clipboard() {
    if __has_file /tmp/.X0-lock && __command_found /usr/bin/VBoxClient; then
        # Xサーバ/Xvfbが起動していること
        DISPLAY=:0 /usr/bin/VBoxClient --clipboard
        if __command_found xsel; then
            alias pbcopy='xsel --display :0 --input --clipboard'
            alias pbpaste='xsel --display :0 --output --clipboard'
        fi
    fi
}

my-zsh::filter() {
    if __command_found fzf; then
        export FZF_DEFAULT_OPTS="--reverse --inline-info --multi --bind 'ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"
    fi
}

ulimit -c unlimited

my-zsh::history
my-zsh::command_line_editting
my-zsh::completion
my-zsh::dir

my-zsh::javascript
my-zsh::golang
my-zsh::java
my-zsh::android
my-zsh::git
my-zsh::clipboard
my-zsh::filter

my-zsh::zplug
my-zsh::alias

# 環境依存の設定を読み込む
# ロケによって異なる設定なので、~/.zshrc_site は共有しない
local zshrc_site=~/.zshrc_site
if __has_file $zshrc_site; then
    . $zshrc_site
fi

# プロファイルを取得
# .zshenv で zmodload zsh/zprof && zprof すること
if __command_found zprof; then
    zprof | less
fi
