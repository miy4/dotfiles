#!/usr/bin/env bash

# ステージング前のローカルでの変更を破棄する
# $@ 破棄するファイル。引数なしの場合はレポジトリ全体を対象にする

if (( $# > 0 )); then
    git checkout "$@"
else
    git checkout .
fi

