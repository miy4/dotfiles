#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    openssl sha1 < /dev/stdin
else
    openssl sha1 "$@"
fi
