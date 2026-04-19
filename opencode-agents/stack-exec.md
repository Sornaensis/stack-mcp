---
description: Eval, runghc, hoogle
mode: subagent
permission:
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
  stack_mcp/stack_eval: true
  stack_mcp/stack_runghc: true
  stack_mcp/stack_hoogle: true
---

# stack-exec

Expression evaluation, runghc, and Hoogle operations.

Your first tool call must be `set_repo` with the absolute project path.
