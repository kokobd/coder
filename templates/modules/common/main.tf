terraform {
  required_providers {
    coder = {
      source = "coder/coder"
    }
  }
}

data "coder_parameter" "docker_image" {
  name         = "docker_image"
  display_name = "Environment"
  description  = "Base environment to use for your workspace"
  default      = "zelinf/coder-nix:latest"
  icon         = "/icon/docker.png"
  type         = "string"
  mutable      = true
  order        = 1
  option {
    name  = "nix"
    value = "zelinf/coder-nix:latest"
  }
  option {
    name  = "ghcup"
    value = "zelinf/coder-ghcup:latest"
  }
  option {
    name  = "rust"
    value = "zelinf/coder-rust:latest"
  }
}

data "coder_parameter" "dotfiles_uri" {
  name         = "dotfiles_uri"
  display_name = "Dotfiles URI"
  description  = <<-EOF
  Dotfiles repo URI (optional)

  see https://dotfiles.github.io
  EOF
  default      = "git@github.com:kokobd/dotfiles.git"
  type         = "string"
  mutable      = true
  order        = 2
}

data "coder_parameter" "dotfiles_branch" {
  name         = "dotfiles_branch"
  display_name = "Dotfiles branch"
  default      = "main"
  type         = "string"
  mutable      = true
  order        = 3
}

data "coder_parameter" "github_repo" {
  name         = "github_repo"
  display_name = "GitHub repository"
  description  = "Owner and repository name, for example octocat/Hello-World"
  type         = "string"
  default      = ""
  mutable      = false
}

data "coder_parameter" "user_secret" {
  name         = "user_secret"
  display_name = "User Secret"
  description  = "A secret string for user-specific needs. Usually is a decryption key for all encrypted files in dotfiles"
  mutable      = true
  default      = ""
  type         = "string"
}

resource "coder_agent" "main" {
  arch                   = var.coder_provisioner.arch
  os                     = "linux"
  startup_script_timeout = 180
  env = {
    DOTFILES_URI      = data.coder_parameter.dotfiles_uri.value != "" ? data.coder_parameter.dotfiles_uri.value : null,
    GITHUB_REPOSITORY = data.coder_parameter.github_repo.value
    LC_ALL            = "C.utf8"
    CODER_REGION      = var.coder_region
    USER_SECRET       = data.coder_parameter.user_secret.value

    # Redirect build cache of common compilers to /workspace
    XDG_CONFIG_HOME = "/workspace/.config"
    XDG_CACHE_HOME  = "/workspace/.cache"
    XDG_STATE_HOME  = "/workspace/.state"
    STACK_ROOT      = "/workspace/.stack"
  }
  dir                     = "/workspace/${basename(data.coder_parameter.github_repo.value)}"
  startup_script_behavior = "blocking"
  startup_script          = <<-EOT
    if [ -d /workspace ]; then
      sudo chown -R coder:coder /workspace
    fi
    if [ ! -d /workspace/.config ]; then
      cp -r ~/.config /workspace/.config
    fi
    echo 'export PATH=/workspace/.bin:$PATH' >> ~/.bashrc
    mkdir -p ~/.ssh
    echo "github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl" > ~/.ssh/known_hosts
    REPO_DIR=/workspace/$(basename "$GITHUB_REPOSITORY")
    if [ ! -z "$GITHUB_REPOSITORY" -a ! -d $REPO_DIR ]; then
      git clone "git@github.com:$GITHUB_REPOSITORY.git" $REPO_DIR
    fi
    if [ -n "$DOTFILES_URI" ]; then
      echo "Installing dotfiles from $DOTFILES_URI"
      coder dotfiles --yes --branch "${data.coder_parameter.dotfiles_branch.value}" "$DOTFILES_URI"
    fi
  EOT
}

resource "coder_metadata" "container_info" {
  count       = var.coder_workspace.start_count
  resource_id = var.container_resource[0].id

  item {
    key   = "image"
    value = data.coder_parameter.docker_image.value
  }
  item {
    key   = "GitHub Repo"
    value = data.coder_parameter.github_repo.value
  }
}
