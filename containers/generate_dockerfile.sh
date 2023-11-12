#!/usr/bin/env bash

set -e

NAME=$1

cd "$(dirname "$0")"

FILENAME="Dockerfile.$NAME"
GENERATED_FILENAME="Dockerfile.$NAME.full"

rm -f "$GENERATED_FILENAME"
cat > "$GENERATED_FILENAME" <<EOF
FROM docker.io/ubuntu:22.04

# Install essential tools
RUN apt-get update -y && \
  apt-get install -y \
    sudo vim build-essential curl man less git screen gpg iputils-ping net-tools
RUN yes | unminimize

# Creat user
RUN groupadd coder && \
  useradd --system --create-home \
  --home-dir /home/coder \
  --shell /bin/bash \
  --gid coder --groups sudo --uid 1000 \
  --password '$1$AxoJEpZ5$QMu6Z6jrv9BKa2und77o90' \
  coder && \
  echo '%coder         ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers

# Prepare /workspace dir. Used only when /workspace is not mounted.
RUN mkdir -p /workspace
EOF

cat >> "$GENERATED_FILENAME" < "$FILENAME"
