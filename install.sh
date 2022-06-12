#!/bin/bash

deps=(curl git tar)

source <(curl -s https://github.com/SeungheonOh/nix.dance/raw/gh-page/choices)

######### User interaction

prompt() {
    read -r "$@" </dev/tty
}

yes-no-prompt() {
    local response
    while :; do
        echo -en "[\033[33m ??? \033[0m] "
        read -r -p "$@ [Y/n] " </dev/tty response
        if [[ "${response}" =~ y|yes ]]; then
            return 0
        elif [[ "${response}" =~ n|no ]]; then
            return 1
        fi
        echo "I didn't quite get that"
    done
}

log-info() {
    echo -e "[\033[32mINFO\033[0m] $*"
}

log-error() {
    echo -e "[\033[31mERROR\033[0m] $*"
}

die() {
    log-error "$@"
    exit 1
}

######### Checks

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

check-nix() {
    if ! hash nix 2>/dev/null; then
	if yes-no-prompt "Nix not found 😱. Do you want to install nix?"; then
            install-nix || die "Canceled, exiting..."
	else
            die "nix not installed. Exiting..."
	fi

	exit 0
    fi    
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

check-envrc() {
    if [ -f .envrc ]; then
	yes-no-prompt ".envrc file found, replace it?" && create-envrc
    else
	create-envrc
    fi
}

check-deps() {
    for d in "${deps[@]}" 
    do
	which $d > /dev/null || {
	    die "$d is not installed or listed in PATH variable. Please install $d and try again."
	}
    done
}    

main() {
    [[ "$TAR" ]] || die "TAR variable not defined, I don't know the template you want"

    # check nix and its env setup
    check-deps
    check-nix
    check-envrc

    log-info "Downloading template"
    TARFile="./$(basename $TAR)"
    echo $TARFile
    curl -O $TAR
    
    log-info "Decompressing the template"
    tar vxzf $TARFile || die "Something gone wrong while decompressing the template."
    rm $TARFile

    # initialize git repo
    if [[ ! -d .git ]]; then
	log-info "Creating a git repo"
	git init .
    fi
    
    log-info "Staging your project in git"
    git add .
g
    log-info "All done :)"
}

choose
main

