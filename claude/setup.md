# Claude Code Environment Setup Guide

> **For Human Readers**: This is a configuration guide for setting up Claude Code plugins, MCP servers, hooks, and auxiliary tools.
>
> **For Claude Code**: This is an executable configuration document. Please execute the command blocks in each Phase sequentially. Refer to the Troubleshooting section if you encounter errors.

## Prerequisite

Run `claude/pre-setup.sh` before starting. It handles:
- System prerequisites check (macOS 13+, git, jq, curl, bun, uv)
- Claude CLI installation
- GLM API configuration (model mapping, env vars)

```bash
./claude/pre-setup.sh
```

## Configuration Overview

This guide will configure the following components:

| Component               | Count | Purpose                                                             |
|-------------------------|-------|---------------------------------------------------------------------|
| **Plugin Marketplaces** | 3     | Official + community + GLM plugin sources                           |
| **Plugins**             | 15    | Development tools, MCP integration, auxiliary features              |
| **MCP Servers**         | 6     | Vision, search, web reader, documentation, browser, advanced search |
| **Hooks**               | 1     | Token optimization (RTK)                                            |
| **Auxiliary Tools**     | 2     | RTK (token savings), claude-hud (status bar)                        |

## Execution Order

**Must execute in order, do not skip or reorder**

```
Step 1: Plugin Marketplaces    -> Add plugin sources
Step 2: Install Plugins        -> Install 15 plugins
Step 3: MCP Servers            -> Configure MCP servers
Step 4: Hooks                  -> Set up automation hooks
Step 5: Auxiliary Tools        -> Install auxiliary tools
Step 6: Verification           -> Verify all configurations
Step 7: Troubleshooting        -> Troubleshoot issues
```

## Official Reference Resources

- **Claude Code**: https://docs.anthropic.com/en/docs/claude-code
- **Claude Plugins Official**: https://github.com/anthropics/claude-plugins-official
- **GLM API Documentation**: https://open.bigmodel.cn/dev/api
- **claude-hud**: https://github.com/jarrodwatts/claude-hud
- **RTK**: https://github.com/rtk-ai/rtk

---

## Step 1: Plugin Marketplaces

### Description

Claude Code supports a plugin system to extend functionality. This guide uses 3 plugin marketplaces:

| Marketplace                 | Purpose              | Source                                                          |
|-----------------------------|----------------------|-----------------------------------------------------------------|
| **claude-plugins-official** | Official plugins     | [GitHub](https://github.com/anthropics/claude-plugins-official) |
| **zai-coding-plugins**      | GLM-specific plugins | Auto-configured by `@z_ai/coding-helper`                        |
| **claude-hud**              | Status bar display   | [GitHub](https://github.com/jarrodwatts/claude-hud)             |

### Add Plugin Marketplaces

```bash
# Add official plugin marketplace
claude plugin marketplace add anthropics/claude-plugins-official

# Add Claude HUD status bar
claude plugin marketplace add jarrodwatts/claude-hud

# Note: zai-coding-plugins is auto-configured by bunx @z_ai/coding-helper in pre-setup.sh

# Verify all marketplaces are added
claude plugin marketplace list
```

---

## Step 2: Install Plugins

### Description

Now install 15 plugins. Each plugin includes installation command and description.

### 2.1 GLM Plugin Marketplace (1 plugin)

> Auto-configured by `bunx @z_ai/coding-helper` in pre-setup.sh. Shown here for reference.

```bash
# glm-plan-usage - GLM Coding Plan usage query
claude plugin install glm-plan-usage@zai-coding-plugins
```

### 2.2 Official Plugin Marketplace (13 plugins)

```bash
# playwright - Browser automation
claude plugin install playwright@claude-plugins-official

# context7 - Document query
claude plugin install context7@claude-plugins-official

# ralph-loop - Loop execution
claude plugin install ralph-loop@claude-plugins-official

# commit-commands - Git commit commands
claude plugin install commit-commands@claude-plugins-official

# claude-md-management - CLAUDE.md management
claude plugin install claude-md-management@claude-plugins-official

# plugin-dev - Plugin development tools
claude plugin install plugin-dev@claude-plugins-official

# hookify - Hook management
claude plugin install hookify@claude-plugins-official

# skill-creator - Skill creation tool
claude plugin install skill-creator@claude-plugins-official

# code-review - Code review
claude plugin install code-review@claude-plugins-official

# feature-dev - Feature development
claude plugin install feature-dev@claude-plugins-official

# code-simplifier - Code simplification
claude plugin install code-simplifier@claude-plugins-official

# frontend-design - Frontend design
claude plugin install frontend-design@claude-plugins-official

# superpowers - Core skill library
claude plugin install superpowers@claude-plugins-official
```

### 2.3 Other Marketplaces (1 plugin)

```bash
# claude-hud - Status bar display
claude plugin install claude-hud@claude-hud
```

### Verify All Plugins

```bash
# List all installed plugins
claude plugin list

# Should see 15 plugins
```

---

## Step 3: MCP Servers

### Description

MCP (Model Context Protocol) server configuration (6 total):

**Auto-configured MCP (via GLM configuration in pre-setup.sh):**
- `web-reader` - Web content reader
- `web-search-prime` - Web search
- `zai-mcp-server` - Multimodal content analysis (vision, OCR, video, etc.)

**Plugin auto-configured MCP:**
- `context7` - Library documentation query (via `context7` plugin)
- `playwright` - Browser automation (via `playwright` plugin)

**Independent configuration MCP (global):**
- `tavily` - Advanced web search (optional)

### Configure tavily-search (Optional)

```bash
# 1. Obtain Tavily API Key
# Visit: https://tavily.com/
# Register account and get API Key

# 2. Add global MCP configuration
claude mcp add --transport http --scope user tavily "https://mcp.tavily.com/mcp/?tavilyApiKey=Your_Tavily_Key"

# 3. Verify configuration
jq '.mcpServers | keys' ~/.claude.json
```

**Reference:** [Tavily Official Site](https://tavily.com/)

### Verify All MCP

```bash
# List all MCP servers
claude mcp list

# Verify GLM MCP configuration
jq '.mcpServers | keys' ~/.claude.json
```

---

## Step 4: Hooks

### Description

Hooks are Claude Code automation scripts:

| Hook    | Trigger           | Purpose                   | Source                                      |
|---------|-------------------|---------------------------|---------------------------------------------|
| **RTK** | PreToolUse (Bash) | Token optimization 60-90% | [rtk-ai/rtk](https://github.com/rtk-ai/rtk) |

### Install RTK

```bash
# Install RTK
brew install rtk

# Initialize RTK Hook (auto-configure to Claude Code)
rtk init -g

# Verify installation
rtk --version
rtk init --show
```

**RTK Features:**
- Automatically rewrites Bash commands to save tokens (60-90% savings)
- Intelligent filtering and compression of command output
- Supports git, gh, cargo, npm, docker, kubectl and other commands

### Verify Hooks

```bash
# Check hooks configuration
jq '.hooks' ~/.claude/settings.json
```

---

## Step 5: Auxiliary Tools

### Description

Auxiliary tools were installed in previous steps, only need verification and configuration:

- **claude-hud** - Installed in Step 2.3
- **RTK** - Installed in Step 4

### Configure claude-hud Statusline

Inside Claude Code interactive session:

```
# Start Claude Code
claude

# Then run the slash command inside the session
/claude-hud:setup

# Optional: Configure HUD display options
/claude-hud:configure
```

**HUD Display Content:**
- Project path, Git status
- Context usage, Token usage
- Tool activity, Agent status, Todo progress

### Verify All Tools

```bash
# 1. Verify claude-hud
claude plugin list | grep claude-hud
jq '.statusLine' ~/.claude/settings.json

# 2. Verify RTK
rtk --version
rtk init --show

# 3. Verify Plugin count
claude plugin list | wc -l
echo "Expected: 15 plugins"
```

---

## Step 6: Verification

### Description

After completing all configurations, perform comprehensive verification.

### Complete Verification Script

```bash
#!/bin/bash

echo "========================================="
echo "Claude Code Environment Verification"
echo "========================================="

# 1. GLM Configuration Check
echo -e "\n[1/7] GLM Configuration Check"
jq -e '.env.ANTHROPIC_BASE_URL' ~/.claude/settings.json >/dev/null 2>&1 && echo "  GLM API endpoint configured" || echo "  GLM configuration missing"

# 2. Plugin Count Check
echo -e "\n[2/7] Plugin Count Check"
PLUGIN_COUNT=$(claude plugin list 2>/dev/null | wc -l | xargs)
[[ "$PLUGIN_COUNT" -ge 15 ]] && echo "  Plugin count: $PLUGIN_COUNT" || echo "  Low plugin count: $PLUGIN_COUNT"

# 3. MCP Check
echo -e "\n[3/7] MCP Check"
MCP_COUNT=$(jq '.mcpServers | length' ~/.claude.json 2>/dev/null)
[[ "$MCP_COUNT" -ge 4 ]] && echo "  MCP count: $MCP_COUNT" || echo "  Low MCP count: $MCP_COUNT"

# 4. Hooks Check
echo -e "\n[4/7] Hooks Check"
jq -e '.hooks.PreToolUse' ~/.claude/settings.json >/dev/null 2>&1 && echo "  Hooks configured" || echo "  Hooks not configured"

# 5. RTK Check
echo -e "\n[5/7] RTK Check"
command -v rtk >/dev/null 2>&1 && echo "  RTK installed: $(rtk --version 2>&1 | head -1)" || echo "  RTK not installed"

# 6. claude-hud Check
echo -e "\n[6/7] claude-hud Check"
jq -e '.statusLine' ~/.claude/settings.json >/dev/null 2>&1 && echo "  claude-hud configured" || echo "  claude-hud not configured"

# 7. Configuration File Format Check
echo -e "\n[7/7] Configuration File Format Check"
jq empty ~/.claude/settings.json 2>/dev/null && echo "  settings.json OK" || echo "  settings.json INVALID"
jq empty ~/.claude.json 2>/dev/null && echo "  .claude.json OK" || echo "  .claude.json INVALID"

echo -e "\n========================================="
echo "Verification Complete"
echo "========================================="
```

**Save and Execute Verification Script:**

```bash
# Save verification script
cat > /tmp/verify-claude-env.sh << 'EOF'
#!/bin/bash
# [Paste the verification script content above]
EOF

# Add execute permission
chmod +x /tmp/verify-claude-env.sh

# Execute verification
/tmp/verify-claude-env.sh
```

---

## Step 7: Troubleshooting

### 7.1 GLM Configuration Issues

**Problem: Invalid GLM API Key**
```bash
# Diagnose
curl -H "Authorization: Bearer YOUR_API_KEY" https://open.bigmodel.cn/api/anthropic

# Solution
# Re-run pre-setup.sh
./claude/pre-setup.sh
```

### 7.2 Plugin Issues

**Problem: Plugin Installation Failed**
```bash
# Check if marketplace is added
claude plugin marketplace list

# Re-add marketplace
claude plugin marketplace add anthropics/claude-plugins-official

# Manually clone marketplace
cd ~/.claude/plugins/marketplaces
git clone https://github.com/anthropics/claude-plugins-official.git
```

**Problem: Plugin Not Loading**
```bash
# Restart Claude Code
# Ctrl+C to exit, then run claude again

# Reinstall Plugin
claude plugin install plugin-name@marketplace
```

### 7.3 MCP Issues

**Problem: MCP Servers Not Showing**
```bash
# List MCP servers
claude mcp list

# Check configuration
jq '.mcpServers' ~/.claude.json

# Restart Claude Code
```

### 7.4 Hooks Issues

**Problem: Hooks Not Working**
```bash
# Check configuration
jq '.hooks' ~/.claude/settings.json

# Check RTK
rtk --version
rtk init --show
```

### 7.5 Network Issues

**Problem: Cannot Access GitHub**
```bash
# Check network connectivity
curl -I https://github.com

# Set proxy (if needed)
export HTTPS_PROXY="http://your-proxy:port"
```

### 7.6 Complete Reset

**Problem: All Configuration Failed**
```bash
# Warning: This will delete all configurations

# 1. Backup important files
cp ~/.claude/settings.json ~/settings.json.backup
cp ~/.claude.json ~/claude.json.backup

# 2. Delete configuration directories
rm -rf ~/.claude/
rm -rf ~/.config/rtk/

# 3. Restart configuration
./claude/pre-setup.sh
# Then follow this guide from Step 1
```

---

**Configuration Complete!**

Your Claude Code environment is now fully configured with:
- GLM model configuration
- 15 plugins
- 6 MCP servers
- RTK token optimization
- claude-hud status bar

Enjoy using Claude Code!
