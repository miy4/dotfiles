#!/usr/bin/env bash

# Bashスクリプトのたたき台をつくる
# $* スクリプトのファイルパス

new_script() {
    cat << _EOF_ > "$1"
#!/usr/bin/env bash

#

_EOF_
    chmod +x "$1"
}

for file in "$@"; do
    new_script "$file"
done
