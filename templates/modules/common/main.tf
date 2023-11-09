terraform {
  required_providers {
    coder = {
      source = "coder/coder"
    }
  }
}

data "coder_parameter" "docker_image" {
  name         = "docker_image"
  display_name = "Docker image"
  description  = "The Docker image will be used to build your workspace."
  default      = "zelinf/coder-nix:latest"
  icon         = "/icon/docker.png"
  type         = "string"
  mutable      = false
}

data "coder_parameter" "dotfiles_uri" {
  name         = "dotfiles_uri"
  display_name = "dotfiles URI"
  description  = <<-EOF
  Dotfiles repo URI (optional)

  see https://dotfiles.github.io
  EOF
  default      = "https://github.com/${var.coder_workspace.owner}/dotfiles"
  type         = "string"
  mutable      = true
  order        = 2
}

data "coder_parameter" "dotfiles_branch" {
  name         = "dotfiles_branch"
  display_name = "dotfiles branch"
  default      = "master"
  type         = "string"
  mutable      = true
}

data "coder_parameter" "gitrepo_uri" {
  name         = "gitrepo_uri"
  display_name = "Git repository URI"
  description  = "Git repository to clone on start"
  type         = "string"
  default      = ""
  mutable      = false
}

resource "coder_agent" "main" {
  arch                   = var.coder_provisioner.arch
  os                     = "linux"
  startup_script_timeout = 180
  env = {
    DOTFILES_URI = data.coder_parameter.dotfiles_uri.value != "" ? data.coder_parameter.dotfiles_uri.value : null,
    GITREPO_URI  = data.coder_parameter.gitrepo_uri.value
    LC_ALL       = "C.utf8"
  }
  dir                     = "/workspace"
  startup_script_behavior = "blocking"
  startup_script          = <<-EOT
    if [ -d /workspace ]; then
      sudo chown -R coder:coder /workspace
    fi
    mkdir -p ~/.ssh
    echo "github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl" > ~/.ssh/known_hosts
    if [ ! -z "$GITREPO_URI" -a -z "$(ls -A /workspace)" ]; then
      git clone "$GITREPO_URI" /workspace
    fi
    if [ -n "$DOTFILES_URI" ]; then
      echo "Installing dotfiles from $DOTFILES_URI"
      coder dotfiles -y "$DOTFILES_URI" --branch "${data.coder_parameter.dotfiles_branch.value}"
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
}
