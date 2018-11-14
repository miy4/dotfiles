#!/usr/bin/env bash

# タグを注釈付きで作成する
# https://git-scm.com/book/en/v2/Git-Basics-Tagging
# $1 タグ名
# $2 注釈

#               -a             -m
git tag --annotate "$1" --message=\""$2"\"
