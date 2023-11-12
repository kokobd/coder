#!/usr/bin/env bash

set -e

NAME=$1

cd "$(dirname "$0")"

rm -rf temp
cp -rL $NAME temp
cd temp
if [ ! -f vars.yml ]; then
  touch vars.yml
fi
coder templates push $NAME -y --variables-file="vars.yml"
rm -rf temp
