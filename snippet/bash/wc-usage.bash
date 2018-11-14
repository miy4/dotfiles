#!/usr/bin/env bash

# wcコマンドのオプションとその出力サンプル
# $* ファイルパス

wc_cat() {
    printf "%s	\033[32;1m| %s\033[m\n" "$(wc -c $1)" "-c, --bytes	print the byte counts"
    printf "%s	\033[32;1m| %s\033[m\n" "$(wc -m $1)" "-m, --chars	print the character counts"
    printf "%s	\033[32;1m| %s\033[m\n" "$(wc -l $1)" "-l, --lines	print the newline counts"
    printf "%s	\033[32;1m| %s\033[m\n" "$(wc -L $1)" "-L, --max-line-length	print the maximum display width"
    printf "%s	\033[32;1m| %s\033[m\n" "$(wc -w $1)" "-w, --words	print the word counts"
}

for file in "$@"; do
    wc_cat "$file"
done
