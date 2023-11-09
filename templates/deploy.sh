#!/usr/bin/env bash

set -e

NAME=$1

cd "$(dirname "$0")"

rm -rf temp
cp -rL $NAME temp
cd temp
coder templates push $NAME -y
rm -rf temp
