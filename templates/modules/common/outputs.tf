
output "container_startup_script" {
  description = "Startup script to run in the container"
  value       = coder_agent.main.init_script
}

output "coder_agent_token" {
  description = "Environment variable CODER_AGENT_TOKEN in container should be set to this value"
  value       = coder_agent.main.token
}

output "docker_image" {
  description = "The docker image chosen by user"
  value       = data.coder_parameter.docker_image.value
}
