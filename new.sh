#!/usr/bin/env bash
set -euxo pipefail

mkdir "$1"
cd "$1"
cabal init -l ISC -e "" -x ""
