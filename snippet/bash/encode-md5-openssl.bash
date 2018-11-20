#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    openssl md5 < /dev/stdin
else
    openssl md5 "$@"
fi
