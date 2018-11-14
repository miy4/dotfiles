#!/usr/bin/env bash

# dateコマンドのいろいろな書式とその出力サンプル

source_user_defined() {
    readonly catalogue=(
        'date +%Y/%m/%d'
        'date +"%Y/%m/%d %T"'
        'date +"%Y/%-m/%-d(%a)"'
        'LC_TIME=C date +"%Y/%-m/%-d(%a)"'
        'LC_TIME=ja_JP.utf8 date +"%EY"'
        'date --iso-8601'
        'date --iso-8601=hours'
        'date --iso-8601=minutes'
        'date --iso-8601=seconds'
        'date --iso-8601=ns'
        'date --rfc-2822'
        'date --rfc-3339=date'
        'date --rfc-3339=seconds'
        'date --rfc-3339=ns'
    )

    for s in "${catalogue[@]}"; do
        printf "%s	\033[32;1m| %s\033[m\n" "$(eval "$s")" "$s"
    done
}

source_date_help() {
    LANG=C date --help | \
        sed -n "/^ *%%/,/^ *%Z/p" | \
        while read -r line; do
            format="${line/% *}"
            [[ $format == "%%" ]] && continue
            [[ $format == "%t" ]] && continue
            [[ $format == "%n" ]] && continue

            example="$(date "+$format")"
            description=$(sed "s/^$format *//" <<< "$line")
            printf "%s	\033[32;1m| %s  # %s\033[m\n" "$example" "date +$format" "$description"
        done
}

{
    source_user_defined
    source_date_help
} | column -t -s $'\t'
