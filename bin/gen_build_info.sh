#!/usr/bin/env bash

set -euf -o pipefail

VERSION="git:$(git describe --always --dirty --no-abbrev)"
GIT_HASH="$(git rev-parse HEAD)"
COMMIT_DATE="$(git show -s --pretty=%cd --date=format:%Y-%m-%d)"
BUILD_HOST="$(hash hostname && hostname -f || cat /etc/hostname)"
BUILD_USER="$(whoami)"
BUILD_TIME="$(date --rfc-3339=seconds)"
OCAML_VERSION="$(ocamlc --version)"

CONTENTS=$(cat << EOF
let version = "$VERSION"
let git_hash = "$GIT_HASH"
let commit_date = "$COMMIT_DATE"
let build_host = "$BUILD_HOST"
let build_user = "$BUILD_USER"
let build_time = "$BUILD_TIME"
let ocaml_version = "$OCAML_VERSION"
EOF
)

echo "$CONTENTS"

