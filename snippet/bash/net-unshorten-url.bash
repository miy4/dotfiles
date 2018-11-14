#!/usr/bin/env bash

# 短縮URLを展開して元のURLを取得する
# $* 短縮URLのリスト
# </dev/stdin 短縮URLのリストの行

unshorten() {
    curl --silent --output /dev/null --head --write-out "%{url_effective}\n" --location "$1"
}

if [ -p /dev/stdin ]; then
    while read -r line; do
        for url in $line; do
            unshorten "$url"
        done
    done < /dev/stdin
else
    for url in "$@"; do
        unshorten "$url"
    done
fi
