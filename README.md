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

Restart Claude Code / VS Code after installation to pick up the new MCP server.
