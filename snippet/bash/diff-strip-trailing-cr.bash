#!/usr/bin/env bash

# 改行文字の違いを無視して差分をとる
# $* ファイル

diff --strip-trailing-cr "$@"
