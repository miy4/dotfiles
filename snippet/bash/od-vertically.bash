#!/usr/bin/env bash

# バイナリとテキスト表現を上下に並べて、ファイルを16進ダンプする
# $* 16進ダンプするファイルのリスト

#                -Ax        -tx1c                  -v
od --address-radix=x --format=x1c --output-duplicates "$@"
