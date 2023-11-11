terraform {
  required_providers {
    coder = {
      source = "coder/coder"
    }
    docker = {
      source = "kreuzwerker/docker"
    }
  }
}

provider "docker" {
}

data "coder_provisioner" "me" {
}

data "coder_workspace" "me" {
}

module "common" {
  source = "./modules/common"

  coder_provisioner  = data.coder_provisioner.me
  coder_workspace    = data.coder_workspace.me
  container_resource = docker_container.workspace
}


resource "docker_volume" "workspace" {
  name = "coder-${data.coder_workspace.me.id}-workspace"
  lifecycle {
    ignore_changes = all
  }
}

resource "docker_container" "workspace" {
  count = data.coder_workspace.me.start_count
  image = module.common.docker_image
  # Uses lower() to avoid Docker restriction on container names.
  name = "coder-${data.coder_workspace.me.owner}-${lower(data.coder_workspace.me.name)}"
  # Hostname makes the shell more user friendly: coder@my-workspace:~$
  hostname = data.coder_workspace.me.name
  # Use the docker gateway if the access URL is 127.0.0.1
  entrypoint = ["sh", "-c", replace(module.common.container_init_script, "/localhost|127\\.0\\.0\\.1/", "host.docker.internal")]
  env        = ["CODER_AGENT_TOKEN=${module.common.coder_agent_token}"]
  user       = "coder"
  host {
    host = "host.docker.internal"
    ip   = "host-gateway"
  }
  volumes {
    container_path = "/workspace"
    volume_name    = docker_volume.workspace.name
    read_only      = false
  }
  networks_advanced {
    name = docker_network.bridge.id
  }
  dns = ["172.17.0.1"]
  # Add labels in Docker to keep track of orphan resources.
  labels {
    label = "coder.owner"
    value = data.coder_workspace.me.owner
  }
  labels {
    label = "coder.owner_id"
    value = data.coder_workspace.me.owner_id
  }
  labels {
    label = "coder.workspace_id"
    value = data.coder_workspace.me.id
  }
  labels {
    label = "coder.workspace_name"
    value = data.coder_workspace.me.name
  }
}

resource "docker_network" "bridge" {
  name       = "coder-${data.coder_workspace.me.id}-bridge"
  attachable = true
  driver     = "bridge"
  internal   = false
  ipv6       = false
}
