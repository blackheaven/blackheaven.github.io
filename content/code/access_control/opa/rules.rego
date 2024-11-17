package rules

import rego.v1

default allow := false

access_request := input.access_request

allow if {
	some file in data.files
	file.path == access_request.path
	file.owner == access_request.user
	some allowed_action in file.permissions.owner
	allowed_action == access_request.action
}

allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	some group in user.groups
	file.group == group
	some allowed_action in file.permissions.group
	allowed_action == access_request.action
}

allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	every group in user.groups {
		file.group != group
	}
	some allowed_action in file.permissions.others
	allowed_action == access_request.action
}

allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	some group in user.groups
	group == "root"
}
