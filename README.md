# stack-mcp

An MCP (Model Context Protocol) server that exposes [Haskell Stack](https://docs.haskellstack.org/) build tool operations as tools for AI assistants like Claude Code and GitHub Copilot.

## Installing

Builds the server and configures both Claude Code and VS Code Copilot:

```
stack run stack-mcp-install
```

To install for a specific target only:

```
stack run stack-mcp-install -- claude
stack run stack-mcp-install -- copilot
```

To update config using the existing installed executable, without rebuilding or recopying the binary:

```
stack run stack-mcp-install -- --config-only
stack run stack-mcp-install -- copilot --config-only
```

When targeting Copilot, `--config-only` still installs the agent files. It only skips rebuilding and reinstalling the `stack-mcp` executable.

Restart Claude Code / VS Code after installation to pick up the new MCP server.
