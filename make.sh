#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function pub_build
(
    wget 'https://packages.lazarus-ide.org/PoweredBy.zip'
    unzip -o 'PoweredBy.zip' -d 'use/PoweredBy'
    wget 'https://packages.lazarus-ide.org/EyeCandyControls.zip'
    unzip -o 'EyeCandyControls.zip' -d 'use/EyeCandyControls'
    wget 'https://packages.lazarus-ide.org/splashabout.zip'
    unzip -o 'splashabout.zip' -d 'use/splashabout'
    git submodule update --init --recursive
    find 'use' -type 'f' -name '*.lpk' -exec lazbuild --add-package-link {} \;
    find 'src' -type 'f' -name '*.lpi' -exec lazbuild --recursive --build-mode=release {} \;
)

function priv_main
(
    set -euo pipefail
    if !(which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    if ((${#})); then
        case ${1} in
            build) pub_build 1>&2 ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
