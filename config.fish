# -*- mode: fish -*-

begin ## Environment variables
  if [ (umask) = "0000" ]
    # WSL does not apply default umask
    umask 022
  end

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

  set PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/sbin /sbin ~/bin
  if [ -d /usr/X11/bin ]
    set PATH /usr/X11/bin $PATH
  end
  if [ -d /usr/local/opt/coreutils/libexec/gnubin ]
    # use coreutils instead of BSD
    set PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
  end
  if [ -d ~/.linuxbrew ]
    set PATH ~/.linuxbrew/bin ~/.linuxbrew/sbin $PATH
  else if [ -d /home/linuxbrew/.linuxbrew ]
    set PATH /home/linuxbrew/.linuxbrew/bin /home/linuxbrew/.linuxbrew/sbin $PATH
  end
  set -gx PATH $PATH
end

begin ## Color scheme
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

begin ## Visual
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
        case .spacemacs
          pygmentize -O encoding=utf-8 -f terminal256 -l emacs-lisp $argv[1]
        case '*'
          if head -n 1 $argv[1] | grep -qE '^#!.*/(ba)?sh'
            pygmentize -O encoding=utf-8 -f terminal256 -l sh $argv[1]
          else
            pygmentize -O encoding=utf-8 -f terminal256 -g $argv[1]
          end
      end
    end
    set -gx LESSOPEN '| source-highlight-pygments %s'
  else if type --quiet src-hilite-lesspipe.sh
    set -gx LESSOPEN '| src-hilite-lesspipe.sh %s'
  end
end

begin ## Prompting
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
    printf "%s> %s" "$__m" "$__n"
  end
end

begin ## Clipboard
  if [ -f /tmp/.X0-lock -a -x /usr/bin/VBoxClient ]
    set -lx DISPLAY :0
    /usr/bin/VBoxClient --clipboard
  end

  if type --quiet xsel
      alias pbcopy  'xsel --input --clipboard'
      alias pbpaste 'xsel --output --clipboard'
  end
end

begin ## Managing dev environment
  if [ -d ~/.anyenv ]
    set -gx PATH ~/.anyenv/bin $PATH
    anyenv init - fish | source
  end
end

begin ## Golang
  # requires: anyenv install goenv
  # requires: goenv install *.*.*
  # requires: goenv global *.*.*
  if type --quiet --no-functions go
    set -gx GOPATH $HOME
    set -gx PATH $PATH $GOPATH/bin (go env GOROOT)/bin
  end
end

begin ## Rust
  if [ -d ~/.cargo/bin ]
    set -gx PATH $PATH ~/.cargo/bin
    set -gx RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src
    set -gx CARGO_HOME ~/.cargo
  end
end

begin ## Node.js
  # requires: anyenv install nodenv
  # requires: nodenv install *.*.*
  # requires: nodenv global *.*.*
  # or
  if [ -x ~/.nodebrew/current/bin/nodebrew ]
     set -gx NODEBREW_ROOT ~/.nodebrew
     set -gx PATH $PATH ./node_modules/.bin $NODEBREW_ROOT/current/bin
  end
end

begin ## Java
  set -l java_home_cmd /usr/libexec/java_home
  if [ -x $java_home_cmd ]
    set -gx JAVA_HOME (eval $java_home_cmd -v 1.8)
  else if type --quiet jenv
    set -gx JAVA_HOME (jenv javahome)
  end
end

begin ## Android
  if [ -d /opt/android-sdk ]
    set -gx ANDROID_HOME /opt/android-sdk
    set -gx PATH $PATH /opt/android-sdk/tools /opt/android-sdk/platform-tools
  else if [ -d /usr/local/opt/android-sdk ]
    set -gx ANDROID_HOME /usr/local/opt/android-sdk
  end
end

begin ## Python
    # requires: anyenv install pyenv
    # requires: pyenv install x.x.x
    # requires: pyenv global x.x.x
    # or
    if [ -d ~/.pyenv ]
        set -gx PYENV_ROOT ~/.pyenv
        set -gx PATH $PYENV_ROOT/bin $PATH
        status --is-interactive; and . (pyenv init - | psub)
    end
end

begin ## Git
  if [ -x /usr/local/opt/git/share/git-core/contrib/diff-highlight/diff-highlight ]
    set -gx PATH $PATH /usr/local/opt/git/share/git-core/contrib/diff-highlight
  else if [ -x /usr/share/git/diff-highlight/diff-highlight ]
    set -gx PATH $PATH /usr/share/git/diff-highlight
  else if [ -x (brew --prefix)/share/git-core/contrib/diff-highlight ]
    set -gx PATH $PATH (brew --prefix)/share/git-core/contrib/diff-highlight
  end
end

begin ## Commandline filter
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
      set -l dir $PWD
      while true
        set dir (string split -r -m1 / $dir)[1]

        if string length --quiet $dir
          echo $dir
        else
          echo '/'
          break
        end
      end | fzf | read line
      if [ -n "$line" ]
        cd $line
      end
    end

    function select_script
        set -l script_dir ~/.snippet
        for file in $script_dir/*.bash
            echo -n (basename $file)"	"
            sed -e 1,2d "$file" | tr "\n" " "
            echo
        end \
        | fzf --exact --preview "cd $script_dir; "'set -l query (echo {q} | tr " " "|"); echo {1} | grep -E "^|$query" -i --color=always; sed -e 1d {1} | grep -E "^|$query" -i --color=always' \
        | cut -f 1 | read script
        if [ -n "$script" ]
            commandline "$script_dir/$script"
        end
    end
  end
end

begin ## Managing environment variables
  # https://github.com/direnv/direnv
  if type --quiet --no-functions direnv
    eval (direnv hook fish)
  end
end

begin ## Aliasing
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

  function new --description 'Create new directories and empty files'
    if not count $argv >/dev/null
      echo "Usage: new FILE..." >&2
      return 1
    end

    for f in $argv
      mkdir -p (dirname $f); and touch $f
    end
  end

  function = --description 'Basic calculator for your terminal'
    echo "$argv" | bc -l
  end
  alias =hex 'printf "%x\n"'
  alias =dec 'printf "%d\n"'

  if type --quiet mdcat
    function mdview --description 'Markdown viewer in terminal'
      mdcat -c yes $argv[1] | less --squeeze-blank-lines --RAW-CONTROL-CHARS --ignore-case
    end
  else if type --quiet pandoc; and type --quiet groff
    function mdview --description 'Markdown viewer in terminal'
      set -lx LESS_TERMCAP_mb (printf "\e[1m")
      set -lx LESS_TERMCAP_md (printf "\e[1;34m")
	    set -lx LESS_TERMCAP_me (printf "\e[0m")
	    set -lx LESS_TERMCAP_se (printf "\e[0m")
	    set -lx LESS_TERMCAP_so (printf "\e[1;33m")
	    set -lx LESS_TERMCAP_ue (printf "\e[24;0m")
	    set -lx LESS_TERMCAP_us (printf "\e[4;32m")

      pandoc -s -f markdown -t man $argv[1] | groff -t -T utf8 -man | sed 1,4d | less
    end
  end

  if type --quiet mdcat; and type --quiet pandoc
    function orgview --description 'Org file viewer in terminal'
      pandoc -s -f org -t markdown $argv[1] | \
        mdcat | \
        less --squeeze-blank-lines --RAW-CONTROL-CHARS --ignore-case
    end
  else if type --quiet pandoc
    function orgview --description 'Org file viewer in terminal'
      pandoc -s -f org -t man $argv[1] | \
      groff -t -T utf8 -man | \
      sed 1,4d | \
      env LESS_TERMCAP_mb=(printf "\e[1m") \
          LESS_TERMCAP_md=(printf "\e[1;34m") \
	        LESS_TERMCAP_me=(printf "\e[0m") \
	        LESS_TERMCAP_se=(printf "\e[0m") \
	        LESS_TERMCAP_so=(printf "\e[1;33m") \
	        LESS_TERMCAP_ue=(printf "\e[24;0m") \
	        LESS_TERMCAP_us=(printf "\e[4;32m") \
          less
    end
  end

  if [ (uname) = "Darwin" ]
    alias suspend '/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'
  end
end

begin ## Key bindings
  function fish_user_key_bindings
    if functions --query select_ghq_repository
      bind \cx\cg select_ghq_repository
    end

    if functions --query select_history
      bind \cx\cr select_history
    end

    if functions --query select_script
      bind \cx\cs select_script
    end
  end
end

begin ## Loading a site local configuration
  set -l config_site ~/.config/fish/config_site.fish
  if [ -r $config_site ]
    source $config_site
  end
end
