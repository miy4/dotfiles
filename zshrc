load_utilities() {
    test_file_exists() { [[ -f $1 ]] }
    test_dir_exists() { [[ -d $1 ]] }
    test_executable() { [[ -x $1 ]] }
    test_readable() { [[ -r $1 ]] }
    test_command_exists() { type "$1" >/dev/null 2>&1 }
    test_null() { [[ -z $1 ]] }
    test_not_null() { [[ -n $1 ]] }
    on_osx() { [[ $(uname) == "Darwin" ]] }
    on_linux() { [[ $(uname) == "Linux" ]] }
}

my_zsh_history() {
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

my_zsh_command_line_editting() {
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
        local st branch color
        STY= LANG=en_US.UTF-8 vcs_info
        st=`git status 2> /dev/null`
        if [[ -z "$st" ]]; then return; fi
        branch="$vcs_info_msg_0_"
        if   [[ -n "$vcs_info_msg_1_" ]]; then color="green"   # staged
        elif [[ -n "$vcs_info_msg_2_" ]]; then color="red"     # unstaged
        elif [[ -n `echo "$st" | grep "^Untracked"` ]]; then color="cyan"  # untracked
        else color="blue"
        fi
        echo " (%F{$color}$branch%f)"
    }
    PROMPT='%F{blue}%n%f@%F{blue}%m%f:%F{blue}%/%f$(prompt_git_current_branch)'$'\n'"%F{magenta}❯%f "
    #RPROMPT="!%F{cyan}%!%f"

    # スニペット
    peco-snippets() {
        BUFFER=$(grep -v "^#" ~/.snippets | peco --query "$LBUFFER" | sed 's/[[:blank:]]*#[^#]*$//')
        CURSOR=$#BUFFER
        zle clear-screen
    }
    zle -N peco-snippets
    bindkey '^xs' peco-snippets
}

my_zsh_completion() {
    autoload -Uz compinit && compinit -C
    zsh_completions=/usr/local/share/zsh-completions
    test_dir_exists $zsh_completions && fpath=($zsh_completions $fpath)

    # 補完候補を一覧表示
    setopt auto_list

    # 補完候補が複数ある時、補完キー連打でメニュー選択するよう切り替わる
    setopt auto_menu
    zstyle ':completion:*:default' menu select=1
    test_not_null $LS_COLORS && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

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

# ディレクトリ移動後にファイル一覧を表示する
# 一覧が長い場合は抜粋する
# http://qiita.com/yuyuchu3333/items/b10542db482c3ac8b059
ls-abbrev() {
    if [[ ! -r $PWD ]]; then
        return
    fi

    local cmd_ls='ls'
    local -a opt_ls
    opt_ls=('-aCF' '--color=always' '--group-directories-first')
    if on_osx; then
        if type gls > /dev/null 2>&1; then
            cmd_ls='gls'
        else
            # -G : Enable colorized output.
            opt_ls=('-aCFG')
        fi
    fi

    local ls_result
    ls_result=$(CLICOLOR_FORCE=1 COLUMNS=$COLUMNS command $cmd_ls ${opt_ls[@]} | sed $'/^\e\[[0-9;]*m$/d')

    local ls_lines=$(echo "$ls_result" | wc -l | tr -d ' ')

    if [ $ls_lines -gt 10 ]; then
        echo "$ls_result" | head -n 5
        echo '...'
        echo "$ls_result" | tail -n 5
        echo "$(command ls -1 -A | wc -l | tr -d ' ') files exist"
    else
        echo "$ls_result"
    fi
}

my_zsh_dir() {
    ## cd -<TAB> でディレクトリスタックを補完候補に出す
    ## http://qiita.com/items/e12e239afdbaaec78ec7
    export DIRSTACKSIZE=100
    setopt autopushd
    zstyle ':completion:*:cd:*' ignore-parents parent pwd
    zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

    # ディレクトリ移動後にchpwd関数で定義した処理を行う
    chpwd() {
        ls-abbrev
    }

    # 同じディレクトリを pushd しない
    setopt pushd_ignore_dups

    # ディレクトリ名だけで cd
    setopt auto_cd

    # ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
    setopt extended_glob
}

my_zsh_alias() {
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

    if test_command_exists src-hilite-lesspipe.sh; then
        alias l="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRi"
        alias lF="LESSOPEN='| src-hilite-lesspipe.sh %s\' less -sNRij10 +F"
    else
        alias l='less -sNRi'
        alias lF='less -sNRij10 +F'
    fi

    alias g='grep --color=auto'

    alias psv='ps auxww'

    mkdir-and-go() { mkdir -p ${1:?missing operand} && cd $_ }

    alias reload='exec zsh -l'

    autoload -Uz zmv
    alias rename='noglob zmv -W'

    alias ec='emacsclient -n'

    alias md2txt='pandoc -f markdown -t plain'
    md2html() {
        local input_md=${1:?input markdown not found}
        local output_html=${2:?output html not found}
        # ~/.pandoc/templates/template-github.html
        # https://gist.github.com/miy4/09db5ec1c7f6b72eb130/raw/81311210b448fbb64b37bc303f7ca3e81489f4e3/template-github.html
        pandoc -s -t html5 --template=template-github.html $input_md -o $output_html
    }

    alias gho='dir=$(ghq list --full-path | peco --query "$LBUFFER") && [[ -n "$dir" ]] && gh-open $dir'

    if test_dir_exists ~/Dropbox/books; then
        alias dropbook='open "$(ls -1d ~/Dropbox/books/* | peco)"'
    fi

    function = {
        bc -l <<< "$@"
    }

    if on_osx; then
        alias suspend='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'
        alias update='sudo softwareupdate -i -a; brew update; brew upgrade --all'
        alias update-cask='brew update && for c in $(brew cask list); do ! brew cask info $c | grep -qF "Not installed" || brew cask install $c; done && brew cask cleanup && for c in /opt/homebrew-cask/Caskroom/*; do versions=($(ls -t $c)) && for v in "${versions[@]:1}"; do \rm -rf "$c/$v"; done; done'
    fi
}

my_zsh_antigen() {
    # https://github.com/zsh-users/antigen
    local antigen_zsh=~/src/github.com/zsh-users/antigen/antigen.zsh
    if test_file_exists $antigen_zsh; then
        source $antigen_zsh
        antigen bundle zsh-users/zsh-syntax-highlighting
        antigen bundle Tarrasch/zsh-bd
        antigen bundle mollifier/anyframe
        export ENHANCD_COMMAND=ed
        antigen bundle b4b4r07/enhancd
        antigen apply
        bindkey '^xr' anyframe-widget-put-history
        bindkey '^xg' anyframe-widget-cd-ghq-repository
    fi
}

my_zsh_zplug() {
    local zplug_zsh=~/src/github.com/b4b4r07/zplug/zplug
    test_file_exists $zplug_zsh || return

    source $zplug_zsh
    zplug "zsh-users/zsh-syntax-highlighting"
    zplug "zsh-users/zsh-history-substring-search"
    zplug "mollifier/anyframe"
    export ENHANCD_COMMAND=ed
    zplug "b4b4r07/enhancd", of:enhancd.sh

    zplug check || zplug install
    zplug load

    if zplug check "zsh-users/zsh-history-substring-search"; then
        bindkey -M emacs '^P' history-substring-search-up
        bindkey -M emacs '^N' history-substring-search-down
    fi

    if zplug check "mollifier/anyframe"; then
        bindkey '^xr' anyframe-widget-put-history
        bindkey '^xg' anyframe-widget-cd-ghq-repository
    fi
}

my_zsh_ruby() {
    # https://github.com/sstephenson/rbenv/
    if test_command_exists rbenv; then
        eval "$(rbenv init -)"
    fi
}

my_zsh_javascript() {
    # https://github.com/hokaccha/nodebrew
    if test_dir_exists /usr/local/var/nodebrew; then
        export NODEBREW_ROOT=/usr/local/var/nodebrew
        export PATH=$PATH:./node_modules/.bin:$NODEBREW_ROOT/current/bin
    elif test_executable ~/.nodebrew/current/bin/nodebrew; then
        export NODEBREW_ROOT=~/.nodebrew
        export PATH=$PATH:./node_modules/.bin:$NODEBREW_ROOT/current/bin
    fi
}

my_zsh_golang() {
    if test_command_exists go; then
        export GOPATH=$HOME
        export PATH=$PATH:$GOPATH/bin:$(go env GOROOT)/bin
    fi
}

my_zsh_java() {
    local java_home_cmd=/usr/libexec/java_home
    if test_executable $java_home_cmd; then
        export JAVA_HOME=$($java_home_cmd -v 1.7)
    fi
}

my_zsh_android() {
    if test_dir_exists /opt/android-sdk; then
        export ANDROID_HOME=/opt/android-sdk
        export PATH=$PATH:/opt/android-sdk/tools:/opt/android-sdk/platform-tools
    elif test_dir_exists /usr/local/opt/android-sdk; then
        export ANDROID_HOME=/usr/local/opt/android-sdk
    fi
}

my_zsh_git() {
    if test_executable /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight; then
        export PATH=$PATH:/usr/local/opt/git/share/git-core/contrib/diff-highlight
    elif test_executable /usr/share/git/diff-highlight/diff-highlight; then
        export PATH=$PATH:/usr/share/git/diff-highlight
    fi
}

my_zsh_clipboard() {
    if test_file_exists /tmp/.X0-lock && test_command_exists /usr/bin/VBoxClient; then
        # Xサーバ/Xvfbが起動していること
        DISPLAY=:0 /usr/bin/VBoxClient --clipboard
    fi
}

load_utilities

# 環境変数
export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=''
export EDITOR='emacsclient --alternate-editor=vi'
export PAGER=less
export MANPAGER='less -X'
export LESSCHARSET=utf-8
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/bin:/usr/sbin:/sbin:/usr/X11/bin:~/bin
if brew --prefix coreutils >/dev/null 2>&1; then
    # use coreutils instead of BSD
    PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
fi
export PATH

fpath=(/usr/local/share/zsh/site-functions $fpath)

ulimit -c unlimited

my_zsh_history
my_zsh_command_line_editting
my_zsh_completion
my_zsh_dir

my_zsh_ruby
my_zsh_javascript
my_zsh_golang
my_zsh_java
my_zsh_android
my_zsh_git
my_zsh_clipboard

#my_zsh_antigen
my_zsh_zplug
my_zsh_alias

# 環境依存の設定を読み込む
# ロケによって異なる設定なので、~/.zshrc_site は共有しない
local zshrc_site=~/.zshrc_site
if test_file_exists $zshrc_site; then
    . $zshrc_site
fi

# プロファイルを取得
# .zshenv で zmodload zsh/zprof && zprof すること
if test_command_exists zprof; then
    zprof | less
fi
