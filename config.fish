# -*- mode: fish -*-
alias : true

: "Environment variables"; and begin
  ulimit -c unlimited

  set -g fish_greeting ''

  set -gx LANG ja_JP.UTF-8
  set -gx LANGUAGE ja_JP.UTF-8
  set -gx LC_ALL ''

  set -gx EDITOR vim

  set -gx PAGER less
  set -gx MANPAGER 'less -X'
  set -gx LESSCHARSET utf-8
  set -gx LESS '-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
  set -gx LS_COLORS 'di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

  if [ (uname) = "Darwin" ]
    set -gx HOMEBREW_NO_ANALYTICS 1
  end

  set PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/sbin /sbin /usr/X11/bin ~/bin
  if [ -d /usr/local/opt/coreutils/libexec/gnubin ]
    # use coreutils instead of BSD
    set PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
  end
  set -gx PATH $PATH
end

: "Color scheme"; and begin
  # Solarized Dark
  set -l base03  002b36
  set -l base02  073642
  set -l base01  586e75
  set -l base00  657b83
  set -l base0   839496
  set -l base1   93a1a1
  set -l base2   eee8d5
  set -l base3   fdf6e3
  set -l yellow  b58900
  set -l orange  cb4b16
  set -l red     dc322f
  set -l magenta d33682
  set -l violet  6c71c4
  set -l blue    268bd2
  set -l cyan    2aa198
  set -l green   859900

  set -g fish_color_normal       $base0
  set -g fish_color_command      $base0
  set -g fish_color_quote        $cyan
  set -g fish_color_redirection  $base0
  set -g fish_color_end          $base0
  set -g fish_color_error        $red
  set -g fish_color_param        $blue
  set -g fish_color_comment      $base01
  set -g fish_color_match        $cyan
  set -g fish_color_search_match "--background=$base02"
  set -g fish_color_operator     $orange
  set -g fish_color_escape       $cyan
  set -g fish_color_hostname     $cyan
  set -g fish_color_cwd          $yellow
end

: "Visual"; and begin
  function man --description "Colorize man page"
	  set -lx LESS_TERMCAP_mb (printf "\e[1m")
    set -lx LESS_TERMCAP_md (printf "\e[1;34m")
	  set -lx LESS_TERMCAP_me (printf "\e[0m")
	  set -lx LESS_TERMCAP_se (printf "\e[0m")
	  set -lx LESS_TERMCAP_so (printf "\e[1;33m")
	  set -lx LESS_TERMCAP_ue (printf "\e[24;0m")
	  set -lx LESS_TERMCAP_us (printf "\e[4;32m")

    command man $argv
  end

  if type --quiet pygmentize
    # requires: pip install Pygments
    # requires: pip install pygments-style-solarized
    function source-highlight-pygments
      switch (basename $argv[1])
        case .zshrc
          pygmentize -O encoding=utf-8 -O style=solarizeddark -f terminal256 -l sh $argv[1]
        case .spacemacs
          pygmentize -O encoding=utf-8 -O style=solarizeddark -f terminal256 -l emacs-lisp $argv[1]
        case '*'
          if head -n 1 $argv[1] | grep -qE '^#!.*/(ba)?sh'
            pygmentize -O encoding=utf-8 -O style=solarizeddark -f terminal256 -l sh $argv[1]
          else
            pygmentize -O encoding=utf-8 -O style=solarizeddark -f terminal256 -g $argv[1]
          end
      end
    end
    set -gx LESSOPEN '| source-highlight-pygments %s'
  else if type src-hilite-lesspipe.sh
    set -gx LESSOPEN '| src-hilite-lesspipe.sh %s'
  end
end

: "Prompting"; and begin
  set __fish_git_prompt_showdirtystate 'yes'
  set __fish_git_prompt_showstashstate 'yes'
  set __fish_git_prompt_showuntrackedfiles 'yes'
  set __fish_git_prompt_showupstream 'yes'
  set __fish_git_prompt_color_branch blue
  set __fish_git_prompt_color_upstream_ahead green
  set __fish_git_prompt_color_upstream_behind red

  function fish_prompt --description 'Write out the prompt'
    if not set -q __fish_prompt_hostname
      set -g __fish_prompt_hostname (hostname | cut -d . -f 1)
    end

    set -l __b (set_color blue)
    set -l __n (set_color normal)
    set -l __m (set_color magenta)
    printf '%s%s%s@%s%s%s:%s%s%s%s\n' "$__b" "$USER" "$__n" "$__b" "$__fish_prompt_hostname" "$__n" "$__b" "$PWD" "$__n" (__fish_git_prompt)
    printf "%s❯ %s" "$__m" "$__n"
  end
end

: "Aliasing"; and begin
  if type --quiet gls
    alias d 'gls -F -G --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
  else if ls --version | grep -q coreutils
    alias d 'ls -F -G --color=auto --group-directories-first --time-style="+ %Y-%m-%d %T"'
  else
    alias d 'ls -F -G'
  end
  alias v  'd -l'
  alias da 'd -a'
  alias va 'v -a'

  alias l  'less -sNRi'
  alias lF 'less -sNRij10 +F'

  alias g 'grep --color=auto'
  alias psv 'ps auxww'
  alias reload 'exec fish -l'
  alias ec 'emacsclient -n'

  function = --description 'Basic calculator for your terminal'
    echo "$argv" | bc -l
  end

  if [ (uname) = "Darwin" ]
    alias suspend '/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'
  end
end

: "Clipboard"; and begin
  if [ -f /tmp/.X0-lock -a -x /usr/bin/VBoxClient ]
    set -lx DISPLAY :0
    /usr/bin/VBoxClient --clipboard

    if type --quiet xsel
      alias pbcopy  'xsel --display :0 --input --clipboard'
      alias pbpaste 'xsel --display :0 --output --clipboard'
    end
  end
end

: "Golang"; and begin
  if type --quiet --no-functions go
    set -gx GOPATH $HOME
    set -gx PATH $PATH $GOPATH/bin (go env GOROOT)/bin
  end
end

: "Rust"; and begin
  if [ -d ~/.cargo/bin ]
    set -gx PATH $PATH ~/.cargo/bin
    set -gx RUST_SRC_PATH ~/src/github.com/rust-lang/rust/src
    set -gx CARGO_HOME ~/.cargo
  end
end

: "Java"; and begin
  set -l java_home_cmd /usr/libexec/java_home
  if [ -x $java_home_cmd ]
    set -gx JAVA_HOME (eval $java_home_cmd -v 1.8)
  end
end

: "Android"; and begin
  if [ -d /opt/android-sdk ]
    set -gx ANDROID_HOME /opt/android-sdk
    set -gx PATH $PATH /opt/android-sdk/tools /opt/android-sdk/platform-tools
  else if [ -d /usr/local/opt/android-sdk ]
    set -gx ANDROID_HOME /usr/local/opt/android-sdk
  end
end

: "Git"; and begin
  if [ -x /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight ]
    set -gx PATH $PATH /usr/local/opt/git/share/git-core/contrib/diff-highlight
  else if [ -x /usr/share/git/diff-highlight/diff-highlight ]
    set -gx PATH $PATH /usr/share/git/diff-highlight
  end
end

: "Commandline filter"; and begin
  # https://github.com/junegunn/fzf
  if type --quiet --no-functions fzf
    set -gx FZF_DEFAULT_OPTS "--reverse --no-sort --inline-info --multi --bind 'ctrl-k:kill-line,ctrl-v:page-down,alt-v:page-up'"

    function select_ghq_repository
      ghq list --full-path | fzf | read line
      builtin cd $line
      commandline -f repaint
    end

    function select_history
      history | fzf | read line
      commandline $line
    end

    function .. --description 'Select an ancestor directory and go back there'
      set -l dir (pwd)
      while string length --quiet $dir
        set -l dir (string split -r -m1 / $dir)[1]

        if string length --quiet $dir
          echo $dir
        else
          echo '/'
        end
      end | fzf | read line
      cd $line
    end

    if type --quiet --no-functions sman
      function select_commandline_snippet
        set -lx SMAN_APPEND_HISTORY false
        set -lx SMAN_SNIPPET_DIR '~/.config/sman/snippets'

        sman ls | fzf --ansi | awk -F " " '{print $1}' | read line
        sman run --copy $line
        if type --quiet pbpaste
          commandline -i -- (pbpaste)
        else if type --quiet xsel
          commandline -i -- (xsel --clipboard)
        end
      end
      alias s select_commandline_snippet
    end
  end
end

: "Managing environment variables"; and begin
  # https://github.com/direnv/direnv
  if type --quiet --no-functions direnv
    eval (direnv hook fish)
  end
end

: "Key bindings"; and begin
  function fish_user_key_bindings
    if functions --query select_ghq_repository
      bind \cx\cg select_ghq_repository
    end

    if functions --query select_history
      bind \cx\cr select_history
    end

    if functions --query select_commandline_snippet
      bind \cx\cs select_commandline_snippet
    end
  end
end

: "Loading a site local configuration"; and begin
  set -l config_site ~/.config/fish/config_site.fish
  if [ -r $config_site ]
    source $config_site
  end
end
