#!/usr/bin/env bash

# Meadow各サーバのタイムゾーン、日時

echo "Europe:  $(TZ=Europe/Berlin LANG=C date +'%Z %Y/%m/%d (%a) %T')"
#echo "Asia:    $(TZ=Asia/Singapore LANG=C date +'%Z %Y/%m/%d (%a) %T')"
echo "Asia:    $(TZ=America/New_York LANG=C date +'%Z %Y/%m/%d (%a) %T')"
echo "US West: $(TZ=America/Los_Angeles LANG=C date +'%Z %Y/%m/%d (%a) %T')"
echo "US East: $(TZ=America/New_York LANG=C date +'%Z %Y/%m/%d (%a) %T')"
