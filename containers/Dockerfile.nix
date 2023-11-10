# Install nix
RUN addgroup --system nixbld \
  && adduser coder nixbld \
  && for i in $(seq 1 30); do useradd -ms /bin/bash nixbld$i &&  adduser nixbld$i nixbld; done \
  && mkdir -m 0755 /nix && chown coder /nix
COPY --chown=coder:coder nix.conf /etc/nix/nix.conf
USER coder
RUN curl https://nixos.org/releases/nix/nix-2.16.1/install | sh
RUN echo '. /home/coder/.nix-profile/etc/profile.d/nix.sh' >> /home/coder/.bashrc && \
  mkdir -p /home/coder/.config/nixpkgs && echo '{ allowUnfree = true; }' >> /home/coder/.config/nixpkgs/config.nix
USER root

# Install direnv
RUN apt-get install -y direnv && \
  echo 'eval "$(direnv hook bash)"' >> /home/coder/.bashrc
USER coder
RUN USER=coder . /home/coder/.nix-profile/etc/profile.d/nix.sh && \
  nix-env -f '<nixpkgs>' -iA nix-direnv && \
  mkdir -p /home/coder/.config/direnv/ && \
  echo 'source $HOME/.nix-profile/share/nix-direnv/direnvrc' > /home/coder/.config/direnv/direnvrc
