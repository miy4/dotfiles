#!/usr/bin/env bash

# パッケージ名や説明に正規表現マッチしたパッケージを挙げる
# $* キーワードのリスト(正規表現)

while (($#)); do
    pacman -Ss "$1"
    shift
done
