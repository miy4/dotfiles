#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    openssl sha256 < /dev/stdin
else
    openssl sha256 "$@"
fi
