terraform {
  required_providers {
    aws = {
      source = "hashicorp/aws"
    }
  }
}

resource "aws_vpc" "main" {
  cidr_block = "10.1.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support = true
}

resource "aws_subnet" "main" {
  for_each = {for idx, val in data.aws_availability_zones.available.zone_ids: idx => val}
  vpc_id = aws_vpc.main.id
  availability_zone_id = each.value
  cidr_block = "10.1.${tonumber(each.key) * 16}.0/20"
}

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id
}

resource "aws_route_table" "main" {
  vpc_id = aws_vpc.main.id
  route {
    gateway_id = aws_internet_gateway.main.id
    cidr_block = "0.0.0.0/0"
  }
}

resource "aws_route_table_association" "subnet" {
  route_table_id = aws_route_table.main.id
  for_each = aws_subnet.main
  subnet_id = each.value.id
}

resource "aws_security_group" "main" {
  description = "allow all network traffic"
  vpc_id = aws_vpc.main.id
  ingress {
    protocol = "-1"
    from_port = 0
    to_port = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    protocol = "-1"
    from_port = 0
    to_port = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
}

data "aws_availability_zones" "available" {
  state = "available"
}
