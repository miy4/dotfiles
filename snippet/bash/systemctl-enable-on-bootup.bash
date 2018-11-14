#!/usr/bin/env bash

# 起動時にユニットを自動で起動するよう有効化する
# $1 ユニット名

if [[ $(systemctl is-enabled "$1") = "enabled" ]]; then
   printf "%s is already enabled to be started on bootup\\n" "$1" 1>&2
   exit 1
fi

systemctl enable "$1"
