#!/usr/bin/env bash
set -euxo pipefail

stack new "$1" simple -p "author-name:nzbr" -p "author-email: " -p "category:aoc2020" -p "github-username:nzbr"
cd "$1"

cat >hie.yaml <<EOF
cradle:
    stack:
EOF
cp -v ../LICENSE LICENSE
