---
description: Build, test, benchmark, haddock, install, clean
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
  stack_mcp/stack_build: true
  stack_mcp/stack_test: true
  stack_mcp/stack_bench: true
  stack_mcp/stack_haddock: true
  stack_mcp/stack_install: true
  stack_mcp/stack_clean: true
  stack_mcp/stack_test_run: true
  stack_mcp/stack_bench_run: true
---

# stack-build

Build, test, benchmark, and documentation operations.

Your first tool call must be `set_repo` with the absolute project path.
