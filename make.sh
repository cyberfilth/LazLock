#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

declare -ra USES=(
    'thirdparty/PoweredBy/poweredby/poweredby.lpk'
    'thirdparty/EC_Controls/eccontrols.lpk'
    'thirdparty/dcpcrypt-2.0.4.1/dcpcrypt.lpk'
    'thirdparty/dcpcrypt-2.0.4.1/dcpcrypt_laz.lpk'
    'thirdparty/splashabout_component/poweredby/poweredby.lpk'
    'thirdparty/splashabout_component/splashabout.lpk'
)

function pub_build
(
    wget 'https://packages.lazarus-ide.org/PoweredBy.zip'
    unzip 'PoweredBy.zip' -d 'thirdparty/PoweredBy'
    for lpk in "${USES[@]}"; do
        lazbuild --add-package-link "${lpk}"
    done
    lazbuild --recursive --build-mode=release 'src/lazlock.lpi'
    strip 'bin/x86_64-linux/Hex'
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
            build) pub_build;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}"
