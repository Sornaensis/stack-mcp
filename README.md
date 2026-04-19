# stack-mcp

An MCP (Model Context Protocol) server that exposes [Haskell Stack](https://docs.haskellstack.org/) build tool operations as tools for AI assistants like Claude Code, GitHub Copilot, and OpenCode.

## Installing

Builds the server and configures Claude Code, VS Code Copilot, and OpenCode:

```
stack run stack-mcp-install
```

To install for a specific target only:

```
stack run stack-mcp-install -- claude
stack run stack-mcp-install -- copilot
stack run stack-mcp-install -- opencode
```

To update config using the existing installed executable, without rebuilding or recopying the binary:

```
stack run stack-mcp-install -- --config-only
stack run stack-mcp-install -- copilot --config-only
stack run stack-mcp-install -- opencode --config-only
```

When targeting Copilot, `--config-only` still installs the agent files. It only skips rebuilding and reinstalling the `stack-mcp` executable.

When targeting OpenCode, the installer also copies the bundled `opencode-agents/*.md` files into `~/.config/opencode/agents`.

Restart Claude Code / VS Code / OpenCode after installation to pick up the new MCP server.
