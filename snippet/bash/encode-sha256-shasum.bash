#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    shasum -a 256 < /dev/stdin
else
    shasum -a 256 "$@"
fi
