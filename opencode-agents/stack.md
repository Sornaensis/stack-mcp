---
description: Haskell Stack orchestrator - routes to subagents
mode: primary
permission:
  task:
    "*": deny
    "stack*": allow
tools:
  bash: false
  edit: false
  read: true
  search: true
  web: true
  todo: true
  stack_mcp/set_repo: true
  stack_mcp/get_repo: true
  stack_mcp/stack_pipeline: true
  stack_mcp/stack_config_read: true
---

# stack

Central orchestrator for Stack operations.

Your first tool call must be `set_repo` with the absolute project path.
