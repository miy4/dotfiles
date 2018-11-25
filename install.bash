#!/usr/bin/env bash

test_dir_exists() { [[ -d ${1:?missing argument} ]]; }
test_file_exists() { [[ -f ${1:?missing argument} ]]; }
test_link_exists() { [[ -h ${1:?missing argument} ]]; }
warn() { echo "$@" 1>&2; }
die() { echo "$@" 1>&2; exit 1; }
echo_red() { echo_colored 31 "$@"; }
echo_green() { echo_colored 32 "$@"; }
echo_yellow() { echo_colored 33 "$@"; }
echo_blue() { echo_colored 34 "$@"; }
echo_colored() {
    local color=$1
    shift
    echo -e "\033[${color}m$@\033[m"
}

link_file() {
    local source=${1:?missing argument}
    local target=${2:?missing argument}

    echo_green "$source -> $target"
    if ! test_file_exists $source; then
        warn "file not found: $source"
        return 1
    elif test_link_exists $target; then
        warn "link already exists: $target"
        return 1
    else
        ln -s $source $target
    fi
}

while (($#)); do
    case $1 in
        --debug) set -o xtrace;;
    esac
    shift
done

this=$(readlink -f $0)
dotfiles_dir=${this%/*}

! test_dir_exists ~/.emacs.d && mkdir ~/.emacs.d && echo_green "mkdir: $_"
! test_dir_exists ~/.emacs.d/snippets && ln -s ${dotfiles_dir}/emacs.d/snippets ~/.emacs.d/snippets && echo_green "ln -s: $_"
! test_dir_exists ~/.peco && mkdir ~/.peco && echo_green "mkdir: $_"
! test_dir_exists ~/.config/fish && mkdir ~/.config/fish && echo_green "mkdir: $_"

link_file ${dotfiles_dir}/zshrc ~/.zshrc
link_file ${dotfiles_dir}/zshenv ~/.zshenv
link_file ${dotfiles_dir}/config.fish ~/.config/fish/config.fish
link_file ${dotfiles_dir}/tmux.conf ~/.tmux.conf
link_file ${dotfiles_dir}/emacs.d/init.el ~/.emacs.d/init.el
link_file ${dotfiles_dir}/spacemacs ~/.spacemacs
link_file ${dotfiles_dir}/peco/config.json ~/.peco/config.json
link_file ${dotfiles_dir}/keysnail.js ~/.keysnail.js
link_file ${dotfiles_dir}/snippets ~/.snippets
link_file ${dotfiles_dir}/gitconfig ~/.gitconfig
