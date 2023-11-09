#!/usr/bin/env bash

set -e

cd "$(dirname "$0")"

rm -rf temp
cp -rL docker temp
cd temp
coder templates push docker -y
