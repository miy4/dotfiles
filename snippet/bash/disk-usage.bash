#!/usr/bin/env bash

# カレントディレクトリを起点にツリー探索して、ディレクトリをファイル容量順に出力する

#       -b                      -n
du --bytes . | sort --numeric-sort
