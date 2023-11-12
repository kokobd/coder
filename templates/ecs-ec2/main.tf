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

data "coder_parameter" "cpu_count" {
  name         = "cpu_count"
  display_name = "CPU count"
  type         = "number"
  default      = 2
  # EC2 fleet won't immediately use new launch template, set this as immutable for now.
  # We should find a way to update instance immediately and being able to set this as mutable.
  mutable = false
}

data "coder_parameter" "memory_gib" {
  name         = "memory_gib"
  display_name = "Memory in GiB"
  type         = "number"
  default      = 8
  mutable      = false
}

data "coder_parameter" "disk_size_gib" {
  name         = "disk_size_gib"
  display_name = "Disk size in GiB"
  type         = number
  default      = 60
  mutable      = false
}

module "common" {
  source = "./modules/common"

  coder_provisioner   = data.coder_provisioner.me
  coder_workspace     = data.coder_workspace.me
  container_resource  = aws_ecs_service.main
  coder_template_name = "ecs-ec2"
}


resource "aws_ecs_service" "main" {
  count               = data.coder_workspace.me.start_count
  name                = "coder-${data.coder_workspace.me.id}"
  desired_count       = 1
  launch_type         = "EC2"
  scheduling_strategy = "DAEMON"
  placement_constraints {
    type       = "memberOf"
    expression = "attribute:coder.workspace_id == ${data.coder_workspace.me.id}"
  }
  task_definition = aws_ecs_task_definition.main.arn
  cluster         = aws_ecs_cluster.main.arn
  depends_on      = [aws_cloudwatch_log_group.container, aws_efs_mount_target.workspace]
}

resource "aws_ecs_cluster" "main" {
  name = "coder-${data.coder_workspace.me.id}"
}

resource "aws_ecs_task_definition" "main" {
  family = "coder-${data.coder_workspace.me.id}"
  container_definitions = jsonencode([
    {
      name       = "main"
      image      = module.common.docker_image
      essential  = true
      entryPoint = ["sh", "-c", module.common.container_init_script]
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
          value = module.common.coder_agent_token
        }
      ]
      mountPoints = [
        {
          sourceVolume  = "workspace",
          containerPath = "/workspace"
        }
      ]
    }
  ])
  network_mode             = "host"
  requires_compatibilities = ["EC2"]
  memory                   = data.coder_parameter.memory_gib.value * 1024 - 700
  volume {
    name = "workspace"
    efs_volume_configuration {
      file_system_id = aws_efs_file_system.workspace.id
      root_directory = "/"
    }
  }
}

resource "aws_efs_file_system" "workspace" {
  tags = {
    Name = "coder-${data.coder_workspace.me.id}-workspace"
  }
  lifecycle {
    ignore_changes = all
  }
  throughput_mode = "bursting"
  lifecycle_policy {
    transition_to_ia = "AFTER_7_DAYS"
  }
  lifecycle_policy {
    transition_to_primary_storage_class = "AFTER_1_ACCESS"
  }
}

resource "aws_efs_mount_target" "workspace" {
  for_each        = toset(data.aws_subnets.main.ids)
  file_system_id  = aws_efs_file_system.workspace.id
  subnet_id       = each.key
  security_groups = [data.aws_security_group.main.id]
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
      volume_size           = data.coder_parameter.disk_size_gib
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
  id = var.security_group_id
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
