---
description: List test suites and benchmark suites
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
  stack_mcp/stack_test_discover: true
  stack_mcp/stack_bench_discover: true
---

# stack-discovery

Discovery-only Stack inspection.

Your first tool call must be `set_repo` with the absolute project path.
