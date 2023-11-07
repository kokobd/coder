terraform {
  required_providers {
    coder = {
      source = "coder/coder"
    }
    aws = {
      source = "hashicorp/aws"
    }
  }
}

data "coder_provisioner" "me" {
}

data "coder_workspace" "me" {
}

data "aws_region" "current" {}


data "coder_parameter" "dotfiles_uri" {
  name         = "dotfiles_uri"
  display_name = "dotfiles URI"
  description  = <<-EOF
  Dotfiles repo URI (optional)

  see https://dotfiles.github.io
  EOF
  default      = "https://github.com/${data.coder_workspace.me.owner}/dotfiles"
  type         = "string"
  mutable      = true
}


data "coder_parameter" "docker_image" {
  name         = "docker_image"
  display_name = "Docker image"
  default      = "zelinf/coder-nix:latest"
}

data "coder_parameter" "cpu_count" {
  name         = "cpu_count"
  display_name = "CPU count"
  type         = "number"
  default      = 2
}

data "coder_parameter" "memory_gib" {
  name         = "memory_gib"
  display_name = "Memory in GiB"
  type         = "number"
  default      = 8
}

resource "coder_agent" "main" {
  arch                    = data.coder_provisioner.me.arch
  os                      = "linux"
  startup_script_timeout  = 180
  env                     = { "DOTFILES_URI" = data.coder_parameter.dotfiles_uri.value != "" ? data.coder_parameter.dotfiles_uri.value : null }
  dir                     = "/workspace"
  startup_script_behavior = "blocking"
  startup_script          = <<-EOT
    set -e
    if [ -n "$DOTFILES_URI" ]; then
      echo "Installing dotfiles from $DOTFILES_URI"
      coder dotfiles -y "$DOTFILES_URI"
    fi
    mkdir -p /workspace
    sudo chown -R coder:coder /workspace
  EOT
}

resource "coder_metadata" "main" {
  count       = data.coder_workspace.me.start_count
  resource_id = aws_ecs_service.main.id

  item {
    key   = "image"
    value = data.coder_parameter.docker_image.value
  }
}


resource "aws_ecs_service" "main" {
  name                = "coder-${data.coder_workspace.me.id}"
  desired_count       = data.coder_workspace.me.start_count
  launch_type         = "EC2"
  scheduling_strategy = "DAEMON"
  placement_constraints {
    type       = "memberOf"
    expression = "attribute:coder.workspace_id == ${data.coder_workspace.me.id}"
  }
  task_definition = aws_ecs_task_definition.main.arn
  cluster         = aws_ecs_cluster.main.arn
  depends_on      = [aws_cloudwatch_log_group.container]
}

resource "aws_ecs_cluster" "main" {
  name = "coder-${data.coder_workspace.me.id}"
}

resource "aws_ecs_task_definition" "main" {
  family = "coder-${data.coder_workspace.me.id}"
  container_definitions = jsonencode([
    {
      name       = "main"
      image      = data.coder_parameter.docker_image.value
      essential  = true
      entryPoint = ["sh", "-c", coder_agent.main.init_script]
      logConfiguration = {
        logDriver = "awslogs",
        options = {
          "awslogs-group" : "${local.container_log_group_name}",
          "awslogs-region" : "${data.aws_region.current.name}",
          "awslogs-create-group" : "true",
          "awslogs-stream-prefix" : "container"
        }
      }
      environment = [
        { name  = "CODER_AGENT_TOKEN",
          value = "${coder_agent.main.token}"
        }
      ]
      # TODO add EFS volume
    }
  ])
  network_mode             = "host"
  requires_compatibilities = ["EC2"]
  memory                   = data.coder_parameter.memory_gib.value * 1024 - 700
  # TODO add EFS volume
}

locals {
  container_log_group_name = "/ecs/container/coder-${data.coder_workspace.me.id}"
}

resource "aws_cloudwatch_log_group" "container" {
  name              = local.container_log_group_name
  retention_in_days = 1
}

resource "aws_ec2_fleet" "main" {
  timeouts {
    delete = "30m"
  }
  launch_template_config {
    launch_template_specification {
      launch_template_id = aws_launch_template.main.id
      version            = aws_launch_template.main.latest_version
    }
    dynamic "override" {
      for_each = data.aws_subnets.main.ids
      content {
        subnet_id = override.value
      }
    }
  }
  target_capacity_specification {
    default_target_capacity_type = "spot"
    total_target_capacity        = data.coder_workspace.me.start_count
  }
  type                = "maintain"
  terminate_instances = true
}

resource "aws_launch_template" "main" {
  block_device_mappings {
    device_name = "/dev/xvda"

    ebs {
      delete_on_termination = true
      iops                  = 3000
      throughput            = 125
      volume_size           = 100
      volume_type           = "gp3"
    }
  }
  update_default_version               = true
  description                          = "Coder ECS"
  image_id                             = data.aws_ami.amazon_linux_2023_ecs.id
  instance_initiated_shutdown_behavior = "terminate"
  instance_requirements {
    allowed_instance_types = ["m7i.*", "r7a.*", "m6i.*", "r6a.*"]

    vcpu_count {
      min = data.coder_parameter.cpu_count.value
    }
    memory_mib {
      min = data.coder_parameter.memory_gib.value * 1024
    }
  }
  key_name = "macbook air"
  user_data = base64encode(<<EOF
#!/bin/bash
    mkdir -p /etc/ecs
    echo 'ECS_CLUSTER=${aws_ecs_cluster.main.name}' >> /etc/ecs/ecs.config
    echo 'ECS_INSTANCE_ATTRIBUTES={"coder.workspace_id":"${data.coder_workspace.me.id}"}' >> /etc/ecs/ecs.config
  EOF
  )
  network_interfaces {
    associate_public_ip_address = true
    device_index                = 0
    delete_on_termination       = true
    security_groups             = [data.aws_security_group.main.id]
  }
  iam_instance_profile {
    arn = aws_iam_instance_profile.main.arn
  }
}

resource "aws_iam_instance_profile" "main" {
  role = "AmazonEC2ContainerServiceforEC2Role"
}

data "aws_security_group" "main" {
  tags = {
    coder = "true"
  }
}

data "aws_subnets" "main" {
  filter {
    name   = "vpc-id"
    values = [data.aws_security_group.main.vpc_id]
  }
}

data "aws_ami" "amazon_linux_2023_ecs" {
  owners      = ["591542846629"]
  most_recent = true
  filter {
    name   = "name"
    values = ["al2023-ami-ecs*"]
  }
  filter {
    name   = "architecture"
    values = ["x86_64"]
  }
  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}
