---
description: Background tasks, interactive GHCi sessions
mode: subagent
permission:
  task:
    "*": deny
tools:
  bash: false
  edit: false
  agent: true
  read: true
  search: true
  web: true
  todo: true
  stack_mcp/set_repo: true
  stack_mcp/get_repo: true
  stack_mcp/task_run: true
  stack_mcp/task_exec: true
  stack_mcp/task_ghci: true
  stack_mcp/task_ghci_eval: true
  stack_mcp/task_read: true
  stack_mcp/task_write: true
  stack_mcp/task_kill: true
  stack_mcp/task_list: true
---

# stack-tasks

Background and interactive task management.

Your first tool call must be `set_repo` with the absolute project path.
