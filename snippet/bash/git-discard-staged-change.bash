#!/usr/bin/env bash

# ステージング後、コミット前の変更を破棄する
# $@ 破棄するファイル。引数なしの場合はレポジトリ全体を対象にする

if (( $# > 0 )); then
    git reset HEAD "$@"
else
    git reset HEAD .
fi

