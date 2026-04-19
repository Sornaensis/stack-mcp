---
description: Add/remove dependencies, modules, ghc-options
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
  stack_mcp/project_dependency: true
  stack_mcp/project_add_module: true
  stack_mcp/project_expose_module: true
  stack_mcp/project_rename_module: true
  stack_mcp/project_remove_module: true
  stack_mcp/project_list_modules: true
  stack_mcp/project_extra_dep: true
  stack_mcp/project_set_ghc_options: true
  stack_mcp/project_extension: true
  stack_mcp/project_add_component: true
  stack_mcp/project_resolve_module: true
---

# stack-edit

Project file and dependency edits.

Your first tool call must be `set_repo` with the absolute project path.
