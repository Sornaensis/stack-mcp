---
description: Paths, query metadata, list tools, upgrade
mode: subagent
permission:
  edit: deny
  write: deny
  task:
    "*": deny
tools:
  bash: false
  edit: false
  read: true
  search: true
  web: true
  todo: true
  stack_mcp/set_repo: true
  stack_mcp/get_repo: true
  stack_mcp/stack_path: true
  stack_mcp/stack_ls_tools: true
  stack_mcp/stack_ide_info: true
  stack_mcp/stack_upgrade: true
---

# stack-info

Path, metadata, and environment queries.

Your first tool call must be `set_repo` with the absolute project path.
