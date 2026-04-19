---
description: List dependencies, snapshots, package index
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
  stack_mcp/stack_ls_dependencies: true
  stack_mcp/stack_ls_snapshots: true
  stack_mcp/stack_ls_globals: true
  stack_mcp/stack_update: true
---

# stack-deps

Dependency inspection and snapshot queries.

Your first tool call must be `set_repo` with the absolute project path.
