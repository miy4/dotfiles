#!/bin/bash

main() {
    local -r target="bundle.my.cmdcat"
    local -r install_dir="${HOME}/.local/share/cmdcat"

    perl -0pe "s#\n+\Z#\0#g" ./*.cmdcat >${target}
    install -m 755 -d "${install_dir}"
    install -m 644 ${target} "${install_dir}/${target}"
    rm ${target}
}

main "$@"
