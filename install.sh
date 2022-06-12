#!/bin/bash

{

######### User interaction

prompt() {
    read -r "$@" </dev/tty
}

yes-no-prompt() {
    local response
    while :; do
        echo -en "[\e[33m ??? \e[0m] "
        read -r -p "$@ [Y/n] " </dev/tty response
        if [[ "${response,,}" =~ y|yes ]]; then
            return 0
        elif [[ "${response,,}" =~ n|no ]]; then
            return 1
        fi
        echo "I didn't quite get that"
    done
}

log-info() {
    echo -e "[\e[32mINFO\e[0m ] $*"
}

log-error() {
    echo -e "[\e[31mERROR\e[0m] $*"
}

die() {
    log-error "$@"
    exit 1
}

######### Config gen

install-nix() {
    log-info "installing nix..."
    select opt in "Multi user install (recomended)" "Single user install"; do
        case "$opt" in
            Multi*)
                sh <(curl -L https://nixos.org/nix/install) --daemon
                return $?
                ;;
            Single*)
                sh <(curl -L https://nixos.org/nix/install) --no-daemon
                return $?
                ;;
            *) continue ;;
        esac
    done
}

create-envrc() {
    log-info CREATING .envrc

    cat <<EOF > .envrc
if ! has nix_direnv_version || ! nix_direnv_version 2.1.1; then
  source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.1.1/direnvrc" "sha256-b6qJ4r34rbE23yWjMqbmu3ia2z4b2wIlZUksBke/ol0="
fi
use flake
EOF
}

[[ "$TAR" ]] || die "TAR variable not defined, I don't know the template you want"

if ! hash nix 2>/dev/null; then
    if yes-no-prompt "Nix not found 😱. Do you want to install nix?"; then
        install-nix || die "Canceled, exiting..."
    else
        die "nix not installed. Exiting..."
    fi

    exit 0
fi

if [ -f .envrc ]; then
    yes-no-prompt ".envrc file found, replace it?" && create-envrc
else
    create-envrc
fi

log-info "Decompressing the template"

wget --quiet "$TAR"
tar vxzf "$(basename $TAR)"
rm "$TAR"

if [[ ! -d .git ]]; then
    log-info "Creating a git repo"
    git init .
fi

log-info "Staging your project in git"

git add --verbose "$(tar -tf "$TAR")"


log-info "All done :)"

}
