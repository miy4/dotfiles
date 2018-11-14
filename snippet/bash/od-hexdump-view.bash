#!/usr/bin/env bash

# バイナリエディタ風にファイルを16進ダンプする
# $* 16進ダンプするファイルのリスト

#                -Ax        -tx1z                  -v
od --address-radix=x --format=x1z --output-duplicates "$@"
