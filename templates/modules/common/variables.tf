variable "coder_provisioner" {
  description = "Current coder provisioner"
  nullable    = false
}

variable "coder_workspace" {
  description = "Current coder workspace"
  nullable    = false
}

variable "container_resource" {
  description = "The terraform resource that provides the container. It must be a list, and each item should have an 'id'"
  nullable    = false
}

variable "coder_region" {
  type = string
  description = "Region that the coder server runs in"
  nullable = false
}