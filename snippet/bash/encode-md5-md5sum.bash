#!/usr/bin/env bash

if [ -p /dev/stdin ]; then
    md5sum < /dev/stdin
else
    md5sum "$@"
fi
