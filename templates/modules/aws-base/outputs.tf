output "security_group_id" {
  description = "ID of the created security group in the VPC. This can be used to find all related VPC resource"
  value       = aws_security_group.main.id
}