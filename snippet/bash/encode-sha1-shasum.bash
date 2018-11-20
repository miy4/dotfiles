#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    shasum -a 1 < /dev/stdin
else
    shasum -a 1 "$@"
fi
