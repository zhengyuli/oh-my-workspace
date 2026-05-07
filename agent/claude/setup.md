# Claude Code Environment Setup Guide

> **For Human Readers**: This is a configuration guide for setting up Claude Code plugins, MCP servers, hooks, and auxiliary tools.
>
> **For Claude Code**: This is an executable configuration document. Please execute the command blocks in each Phase sequentially. Refer to the Troubleshooting section if you encounter errors.

## Prerequisite

**IMPORTANT**: Run `agent/claude/pre-setup.sh` manually in your terminal BEFORE entering Claude Code. Claude Code MUST NOT execute this script — it requires interactive input (API key, plan selection) that only a human can provide.

It handles:
- System prerequisites check (macOS 13+, git, jq, curl, bun, uv)
- Claude CLI installation
- GLM API configuration (model mapping, env vars)

```bash
# Run this in your terminal, NOT inside Claude Code
./agent/claude/pre-setup.sh
```

After `pre-setup.sh` completes successfully, open Claude Code and follow Steps 1–7 below.

## Configuration Overview

This guide will configure the following components:

| Component               | Count | Purpose                                                             |
|-------------------------|-------|---------------------------------------------------------------------|
| **Plugin Marketplaces** | 5     | Official + community + GLM plugin sources                           |
| **Plugins**             | 20    | Development tools, MCP integration, auxiliary features, Obsidian    |
| **MCP Servers**         | 7     | Vision, search, web reader, documentation, browser, advanced search, document conversion |
| **Hooks**               | 1     | Token optimization (RTK)                                            |
| **Auxiliary Tools**     | 3     | RTK (token savings), claude-hud (status bar), Happy (mobile client) |

## Execution Order

**Must execute in order, do not skip or reorder**

> **Idempotent**: All commands in this guide are safe to re-run.
> Plugin marketplace add, plugin install, and MCP add skip or overwrite
> existing entries without error.

**Claude Code starts here** — execute Steps 1–7 sequentially:

```
Step 1: Plugin Marketplaces    -> Add plugin sources
Step 2: Install Plugins        -> Install 20 plugins
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
- **Obsidian Skills**: https://github.com/kepano/obsidian-skills
- **Happy**: https://happy.engineering
- **last30days**: https://github.com/mvanhorn/last30days-skill

---

## Step 1: Plugin Marketplaces

### Description

Claude Code supports a plugin system to extend functionality. This guide uses 5 plugin marketplaces (+ 1 auto-configured):

| Marketplace                 | Purpose              | Source                                                          |
|-----------------------------|----------------------|-----------------------------------------------------------------|
| **claude-plugins-official** | Official plugins     | [GitHub](https://github.com/anthropics/claude-plugins-official) |
| **zai-coding-plugins**      | GLM-specific plugins | Auto-configured by `@z_ai/coding-helper`                        |
| **claude-hud**              | Status bar display   | [GitHub](https://github.com/jarrodwatts/claude-hud)             |
| **obsidian-skills**         | Obsidian vault tools | [GitHub](https://github.com/kepano/obsidian-skills)             |
| **caveman**                 | Token compression    | [GitHub](https://github.com/JuliusBrussee/caveman)              |
| **last30days-skill**        | Topic research       | [GitHub](https://github.com/mvanhorn/last30days-skill)          |

### Add Plugin Marketplaces

```bash
# Add official plugin marketplace
claude plugin marketplace add anthropics/claude-plugins-official

# Add Claude HUD status bar
claude plugin marketplace add jarrodwatts/claude-hud

# Add Obsidian skills
claude plugin marketplace add kepano/obsidian-skills

# Add Caveman token-compression plugin
claude plugin marketplace add JuliusBrussee/caveman

# Add last30days research skill
claude plugin marketplace add mvanhorn/last30days-skill

# Note: zai-coding-plugins is auto-configured by bunx @z_ai/coding-helper in pre-setup.sh

# Verify all marketplaces are added
claude plugin marketplace list
```

---

## Step 2: Install Plugins

### Description

Now install 20 plugins. Each plugin includes installation command and description.

### 2.1 GLM Plugin Marketplace (1 plugin)

> Auto-configured by `bunx @z_ai/coding-helper` in pre-setup.sh. Shown here for reference.

```bash
# glm-plan-usage - GLM Coding Plan usage query
claude plugin install glm-plan-usage@zai-coding-plugins
```

### 2.2 Official Plugin Marketplace (15 plugins)

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

# security-guidance - Security risk detection on file edits (XSS, injection, etc.)
claude plugin install security-guidance@claude-plugins-official

# pr-review-toolkit - Comprehensive PR review (comments, tests, error handling, code quality)
claude plugin install pr-review-toolkit@claude-plugins-official
```

### 2.3 last30days Marketplace (1 plugin)

```bash
# last30days - Research topics across Reddit, X, YouTube, HN, Polymarket, GitHub etc.
claude plugin install last30days@last30days-skill
```

### 2.4 claude-hud Marketplace (1 plugin)

```bash
# claude-hud - Status bar display
claude plugin install claude-hud@claude-hud
```

### 2.5 Obsidian Skills Marketplace (1 plugin)

```bash
# obsidian - Obsidian vault skills (markdown, bases, canvas, CLI, defuddle)
claude plugin install obsidian@obsidian-skills
```

### 2.6 Caveman Marketplace (1 plugin)

```bash
# caveman - Token-efficient terse output (~75% token reduction)
claude plugin install caveman@caveman
```

### Verify All Plugins

```bash
# List all installed plugins
claude plugin list

# Should see 20 plugins
```

---

## Step 3: MCP Servers

### Description

MCP (Model Context Protocol) server configuration (7 total):

**Auto-configured MCP (via GLM configuration in pre-setup.sh):**
- `web-reader` - Web content reader
- `web-search-prime` - Web search
- `zai-mcp-server` - Multimodal content analysis (vision, OCR, video, etc.)

**Plugin auto-configured MCP:**
- `context7` - Library documentation query (via `context7` plugin)
- `playwright` - Browser automation (via `playwright` plugin)

**Independent configuration MCP (global):**
- `markitdown` - Document/file conversion to Markdown (PDF, DOCX, PPTX, XLSX, etc.)
- `tavily` - Advanced web search (optional)

### Configure tavily-search (Optional)

```bash
# 1. Obtain Tavily API Key
# Visit: https://tavily.com/
# Register account and get API Key

# 2. Add global MCP configuration (replace <YOUR_KEY> with actual key)
TAVILY_KEY="<YOUR_TAVILY_KEY>"  # ← Replace with your actual key
claude mcp add --transport http --scope user tavily \
  "https://mcp.tavily.com/mcp/?tavilyApiKey=${TAVILY_KEY}"

# 3. Verify configuration
jq '.mcpServers | keys' ~/.claude.json
```

**Reference:** [Tavily Official Site](https://tavily.com/)

### Configure markitdown

```bash
# 1. Install markitdown CLI (standalone file conversion tool)
uv tool install markitdown

# 2. Install markitdown-mcp (MCP server for Claude Code)
uv tool install markitdown-mcp

# 3. Add global MCP configuration
claude mcp add --scope user markitdown markitdown-mcp

# 4. Verify
markitdown --version
claude mcp list | grep markitdown
```

**Reference:** [MarkItDown GitHub](https://github.com/microsoft/markitdown)

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

- **Happy** - Claude Code mobile/web client
- **claude-hud** - Installed in Step 2.4
- **RTK** - Installed in Step 4

### Install Happy

```bash
# Install Happy CLI globally via bun
bun install -g happy

# Trust the package (required by bun for global packages)
# Uses XDG_DATA_HOME (falls back to ~/.local/share) to locate bun global dir
BUN_GLOBAL_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/bun/install/global"
if [[ -d "$BUN_GLOBAL_DIR" ]]; then
  cd "$BUN_GLOBAL_DIR" && bun pm trust happy
else
  echo "Warning: bun global dir not found at $BUN_GLOBAL_DIR"
  echo "Please run manually: cd <bun-global-dir> && bun pm trust happy"
fi

# Verify installation
happy --version
```

> **Note**: `bun pm trust` is required because bun restricts lifecycle scripts for globally installed packages. Without this step, Happy may fail to run properly. If the automatic trust fails (e.g. non-standard bun install path), run `bun pm -g bin` to find your global directory, then `cd` into it and run `bun pm trust happy`.

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
# 1. Verify Happy
happy --version

# 2. Verify claude-hud
claude plugin list | grep claude-hud
jq '.statusLine' ~/.claude/settings.json

# 3. Verify RTK
rtk --version
rtk init --show

# 4. Verify Plugin count
claude plugin list 2>/dev/null | grep -c '✔ enabled'
printf 'Expected: 20 plugins\n'
```

---

## Step 6: Verification

### Description

After completing all configurations, perform comprehensive verification.

### Complete Verification Script

```bash
#!/usr/bin/env bash
set -euo pipefail

readonly SCRIPT_NAME="Claude Code Environment Verification"
readonly MIN_PLUGIN_COUNT=20
readonly MIN_MCP_COUNT=5

# --- Logging ---
_pass() { printf '  ✓ %s\n' "$*"; }
_fail() { printf '  ✗ %s\n' "$*" >&2; }

printf '=========================================\n'
printf ' %s\n' "$SCRIPT_NAME"
printf '=========================================\n'

# 1. GLM Configuration Check
printf '\n[1/9] GLM Configuration Check\n'
if jq -e '.env.ANTHROPIC_BASE_URL' ~/.claude/settings.json >/dev/null 2>&1; then
  _pass "GLM API endpoint configured"
else
  _fail "GLM configuration missing"
fi

# 2. Plugin Count Check
printf '\n[2/9] Plugin Count Check\n'
if command -v claude >/dev/null 2>&1; then
  PLUGIN_COUNT="$(claude plugin list 2>/dev/null | grep -c '✔ enabled')"
  if (( PLUGIN_COUNT >= MIN_PLUGIN_COUNT )); then
    _pass "Plugin count: $PLUGIN_COUNT"
  else
    _fail "Low plugin count: $PLUGIN_COUNT (expected >= $MIN_PLUGIN_COUNT)"
  fi
else
  _fail "claude CLI not in PATH"
fi

# 3. MCP Check
printf '\n[3/9] MCP Check\n'
if [[ -f ~/.claude.json ]]; then
  MCP_COUNT="$(jq '.mcpServers | length' ~/.claude.json 2>/dev/null)"
  if (( MCP_COUNT >= MIN_MCP_COUNT )); then
    _pass "MCP count: $MCP_COUNT (3 GLM + 1+ plugins/tavily)"
  else
    _fail "Low MCP count: $MCP_COUNT (expected >= $MIN_MCP_COUNT)"
  fi
else
  _fail "~/.claude.json not found"
fi

# 4. Hooks Check
printf '\n[4/9] Hooks Check\n'
if jq -e '.hooks.PreToolUse' ~/.claude/settings.json >/dev/null 2>&1; then
  _pass "Hooks configured"
else
  _fail "Hooks not configured"
fi

# 5. RTK Check
printf '\n[5/9] RTK Check\n'
if command -v rtk >/dev/null 2>&1; then
  _pass "RTK installed: $(rtk --version 2>&1 | head -1)"
else
  _fail "RTK not installed"
fi

# 6. Happy Check
printf '\n[6/9] Happy Check\n'
if command -v happy >/dev/null 2>&1; then
  _pass "Happy installed: $(happy --version 2>&1 | head -1)"
else
  _fail "Happy not installed"
fi

# 7. claude-hud Check
printf '\n[7/9] claude-hud Check\n'
if jq -e '.statusLine' ~/.claude/settings.json >/dev/null 2>&1; then
  _pass "claude-hud configured"
else
  _fail "claude-hud not configured (run /claude-hud:setup)"
fi

# 8. MarkItDown Check
printf '\n[8/9] MarkItDown Check\n'
if command -v markitdown-mcp >/dev/null 2>&1; then
  _pass "markitdown-mcp installed"
else
  _fail "markitdown-mcp not installed (run: uv tool install markitdown-mcp)"
fi

# 9. Configuration File Format Check
printf '\n[9/9] Configuration File Format Check\n'
if jq empty ~/.claude/settings.json 2>/dev/null; then
  _pass "settings.json OK"
else
  _fail "settings.json INVALID"
fi
if jq empty ~/.claude.json 2>/dev/null; then
  _pass ".claude.json OK"
else
  _fail ".claude.json INVALID"
fi

printf '\n=========================================\n'
printf ' Verification Complete\n'
printf '=========================================\n'
```

**Save and Execute:**

```bash
# Option 1: Copy the script above and pipe to bash
# (select the verification script block, copy to clipboard, then run)
pbpaste | bash

# Option 2: Save to file and execute
# (copy the script block, then save and run)
pbpaste > /tmp/verify-claude-env.sh
chmod +x /tmp/verify-claude-env.sh
/tmp/verify-claude-env.sh
```

---

## Step 7: Troubleshooting

### 7.1 GLM Configuration Issues

**Problem: Invalid GLM API Key**
```bash
# Diagnose (replace <YOUR_API_KEY> with actual key)
GLM_KEY="<YOUR_API_KEY>"  # ← Replace with your actual key
curl -H "Authorization: Bearer ${GLM_KEY}" https://open.bigmodel.cn/api/anthropic

# Solution
# Re-run agent/claude/pre-setup.sh
./agent/claude/pre-setup.sh
```

### 7.2 Plugin Issues

**Problem: Plugin Installation Failed**
```bash
# Check if marketplace is added
claude plugin marketplace list

# Re-add marketplace
claude plugin marketplace add anthropics/claude-plugins-official

# Manually clone marketplace (if re-add fails)
if [[ -d ~/.claude/plugins/marketplaces ]]; then
  cd ~/.claude/plugins/marketplaces
  git clone https://github.com/anthropics/claude-plugins-official.git
else
  printf 'error: directory not found: ~/.claude/plugins/marketplaces\n' >&2
fi
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

> **Warning**: This deletes ALL Claude Code data — CLI binary, plugins, MCP servers, hooks, and settings. You must re-run `pre-setup.sh` and all setup steps from scratch.

```bash
# 1. Backup important files (if still readable)
if [[ -f ~/.claude/settings.json ]]; then
  cp ~/.claude/settings.json ~/settings.json.backup
fi
if [[ -f ~/.claude.json ]]; then
  cp ~/.claude.json ~/claude.json.backup
fi

# 2. Delete all Claude Code data (CLI + config + plugins)
rm -rf ~/.claude/
rm -rf ~/.config/rtk/

# 3. Reinstall CLI + GLM config
./agent/claude/pre-setup.sh

# 4. Re-apply all setup steps (from Step 1)
```

---

**Configuration Complete!**

Your Claude Code environment is now fully configured with:
- GLM model configuration
- 20 plugins
- 7 MCP servers
- RTK token optimization
- claude-hud status bar
- Happy mobile client

Enjoy using Claude Code!
