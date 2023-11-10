all: nix ghcup
.PHONY: all

nix: Dockerfile.nix nix.conf
	./generate_dockerfile.sh nix
	docker build -t zelinf/coder-nix:latest -f Dockerfile.nix.full .
.PHONY: nix

ghcup: Dockerfile.ghcup
	./generate_dockerfile.sh ghcup
	docker build -t zelinf/coder-ghcup:latest -f Dockerfile.ghcup.full .
.PHONY: ghcup
