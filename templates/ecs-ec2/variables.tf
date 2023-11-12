variable "security_group_id" {
  type = string
  description = "The security group to connect to. This determines which VPC and subnets to use."
  nullable = false
}