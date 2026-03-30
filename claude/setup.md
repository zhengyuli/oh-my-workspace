# Claude Code Environment Setup Guide

> **For Human Readers**: This is a complete configuration guide documenting how to set up a Claude Code environment from scratch. Each step includes explanations, commands, and verification methods.
>
> **For Claude Code**: This is an executable configuration document. Please execute the command blocks in each Phase sequentially. Refer to the Troubleshooting section if you encounter errors.

## Configuration Overview

This guide will configure the following components:

| Component           | Count | Purpose                                                        |
|---------------------|-------|----------------------------------------------------------------|
| **GLM Model**       | 1     | Use Zhipu GLM to replace Anthropic Claude API                  |
| **Plugin Marketplaces** | 3  | Official + community + GLM plugin sources                     |
| **Plugins**         | 15    | Development tools, MCP integration, auxiliary features         |
| **MCP Servers**     | 6     | Vision, search, web reader, documentation, browser, advanced search |
| **Hooks**           | 1     | Token optimization (RTK)                                       |
| **Auxiliary Tools** | 2     | RTK (token savings), claude-hud (status bar)                   |

## Execution Order

**Must execute in order, do not skip or reorder**

```
Step 1: Prerequisites Check    -> Verify system environment
Step 2: GLM Configuration      -> Configure GLM API
Step 3: Plugin Marketplaces    -> Add plugin sources
Step 4: Install Plugins        -> Install 15 plugins
Step 5: MCP Servers            -> Configure MCP servers
Step 6: Hooks                  -> Set up automation hooks
Step 7: Auxiliary Tools        -> Install auxiliary tools
Step 8: Verification           -> Verify all configurations
Step 9: Troubleshooting        -> Troubleshoot issues
```

## Official Reference Resources

- **Claude Code**: https://docs.anthropic.com/en/docs/claude-code
- **Claude Plugins Official**: https://github.com/anthropics/claude-plugins-official
- **GLM API Documentation**: https://open.bigmodel.cn/dev/api
- **claude-hud**: https://github.com/jarrodwatts/claude-hud
- **RTK**: https://github.com/rtk-ai/rtk

---

## Step 1: Prerequisites Check

### Description

This guide assumes you have already installed the basic tools via Homebrew (see `pkg/homebrew/Brewfile`).
Before starting configuration, verify the following tools are available:

| Tool           | Purpose                                | Source                       |
|----------------|----------------------------------------|------------------------------|
| **claude CLI** | Claude Code command line tool          | Official native installation |
| **git**        | Clone plugin repositories              | Homebrew                     |
| **jq**         | JSON processing (required for hooks)   | Homebrew                     |
| **curl**       | Download tool                          | System default               |
| **bun**        | JavaScript runtime and package manager | Homebrew (oven-sh/bun/bun)   |
| **uv**         | Python package manager                 | Homebrew                     |

**System Requirements:** macOS 13.0+

### Verification Commands

```bash
# Check operating system
echo "=== Operating System ==="
uname -s  # Should show Darwin (macOS)

# Check macOS version
sw_vers -productVersion  # Needs 13.0 or higher

# Check required tools (installed via Homebrew)
echo -e "\n=== Required Tools (from Homebrew) ==="
command -v git >/dev/null 2>&1 && echo "git: $(git --version | awk '{print $3}')" || echo "git: NOT INSTALLED"
command -v jq >/dev/null 2>&1 && echo "jq: $(jq --version)" || echo "jq: NOT INSTALLED"
command -v curl >/dev/null 2>&1 && echo "curl: $(curl --version | head -1 | awk '{print $2}')" || echo "curl: NOT INSTALLED"
command -v bun >/dev/null 2>&1 && echo "bun: $(bun --version)" || echo "bun: NOT INSTALLED"
command -v uv >/dev/null 2>&1 && echo "uv: $(uv --version | head -1)" || echo "uv: NOT INSTALLED"

# Check claude CLI (needs separate installation)
echo -e "\n=== Claude Code CLI ==="
command -v claude >/dev/null 2>&1 && echo "claude: $(claude --version)" || echo "claude: NOT INSTALLED (will install in next step)"

# Check network connectivity
echo -e "\n=== Network ==="
curl -s --max-time 5 https://api.github.com >/dev/null 2>&1 && echo "GitHub API accessible" || echo "Cannot reach GitHub API"
```

### Expected Results

- git, jq, curl, bun, uv (from Homebrew, should be installed)
- claude (needs installation, see next step)

If Homebrew tools are missing, run:
```bash
brew bundle --file=/path/to/oh-my-workspace/pkg/homebrew/Brewfile
```

### Install Claude CLI (Official Recommended Method)

```bash
# macOS native installation (zero dependencies)
curl -fsSL https://claude.ai/install.sh | bash

# Verify installation
claude --version
```

---

## Step 2: GLM Configuration

### Description

Configure Claude Code to use Zhipu GLM model instead of Anthropic API.
Use the official Zhipu interactive configuration tool to automatically complete:
- GLM API configuration (URL, Token)
- Model mapping (GLM-4.5-air, GLM-5-turbo, GLM-5.1)
- MCP server configuration (vision, search, web reader)
- Environment variable settings

### Prerequisite: Obtain GLM API Key

1. Visit [Zhipu Open Platform](https://open.bigmodel.cn/)
2. Click "Register/Login" in the top right corner
3. Go to [API Keys page](https://open.bigmodel.cn/user-center/apikeys)
4. Create a new API Key and save it

**Reference Documentation:**
- [GLM API Official Documentation](https://open.bigmodel.cn/dev/api)
- [Claude Code Configuration Guide](https://docs.bigmodel.cn/cn/guide/develop/claude)

### Configuration Command

```bash
# Run configuration helper using bunx
bunx @z_ai/coding-helper

# Follow the interactive prompts to complete configuration:
# 1. Select interface language
# 2. Enter GLM API Key
# 3. Select coding plan
# 4. Choose MCP servers to install (vision, search, web reader)
```

### Model Mapping

Claude Code internal models correspond to GLM models:

| Claude Model | GLM Model     | Purpose                                 |
|--------------|---------------|-----------------------------------------|
| Haiku        | `glm-4.5-air` | Fast response, lightweight tasks        |
| Sonnet       | `glm-5-turbo` | Balanced performance, daily development |
| Opus         | `glm-5.1`     | Complex tasks, high quality output      |

**Note:** GLM-5.1 usage is calculated as "3x during peak hours, 2x during off-peak hours", recommended for complex tasks.

### Default Model & Model Environment Variables

The `@z_ai/coding-helper` may not configure the default model and model environment variables. These must be set manually — they are **required** for Claude Code to route requests to the correct GLM models.

```bash
# Set default model to Sonnet with extended context (1M tokens)
jq '.model = "sonnet[1m]"' "$HOME/.claude/settings.json" > /tmp/claude-settings.json && mv /tmp/claude-settings.json "$HOME/.claude/settings.json"

# Set model environment variables (map Claude internal model names to GLM models)
jq '.env.ANTHROPIC_DEFAULT_HAIKU_MODEL = "glm-4.5-air" | \
    .env.ANTHROPIC_DEFAULT_SONNET_MODEL = "glm-5-turbo" | \
    .env.ANTHROPIC_DEFAULT_OPUS_MODEL = "glm-5.1"' "$HOME/.claude/settings.json" > /tmp/claude-settings.json && mv /tmp/claude-settings.json "$HOME/.claude/settings.json"
```

| Setting | Value | Purpose |
|---------|-------|---------|
| `model` | `"sonnet[1m]"` | Default model: Sonnet with 1M context window |
| `ANTHROPIC_DEFAULT_HAIKU_MODEL` | `"glm-4.5-air"` | Maps Haiku requests to GLM-4.5-air |
| `ANTHROPIC_DEFAULT_SONNET_MODEL` | `"glm-5-turbo"` | Maps Sonnet requests to GLM-5-turbo |
| `ANTHROPIC_DEFAULT_OPUS_MODEL` | `"glm-5.1"` | Maps Opus requests to GLM-5.1 |

### Critical Environment Variables

The following three are **critical for GLM API compatibility** — do NOT remove or modify them:

| Variable | Value | Why It Matters |
|----------|-------|----------------|
| `ENABLE_TOOL_SEARCH` | `"0"` | Disables Claude Code's built-in tool search, preventing conflicts with GLM API's tool handling |
| `CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS` | `"1"` | Prevents experimental Claude features from using APIs not supported by GLM |
| `CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC` | `"1"` | Stops non-essential requests to Anthropic servers (telemetry, etc.) that would fail with GLM |

> **Warning:** Removing these variables will cause compatibility issues with the GLM API endpoint, including failed requests and unexpected errors.

### Verify Configuration

```bash
# Check configuration file
echo "=== Claude Code Settings ==="
jq '.env | {
  ANTHROPIC_BASE_URL,
  ANTHROPIC_DEFAULT_HAIKU_MODEL,
  ANTHROPIC_DEFAULT_SONNET_MODEL,
  ANTHROPIC_DEFAULT_OPUS_MODEL,
  ENABLE_TOOL_SEARCH,
  CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS,
  CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC,
  API_TIMEOUT_MS
}' "$HOME/.claude/settings.json"

# Check model setting
echo -e "\n=== Model Setting ==="
jq '.model' "$HOME/.claude/settings.json"

# Check if API Key is set (show only first 10 characters)
echo -e "\n=== API Key Status ==="
API_KEY=$(jq -r '.env.ANTHROPIC_AUTH_TOKEN' "$HOME/.claude/settings.json")
echo "API Key: ${API_KEY:0:10}..."
```

**Test Connection:**

```bash
# Reopen terminal window to apply configuration
# Then start Claude Code
claude

# Verify model in Claude Code
# Enter: /status
# Should display GLM model information
```

---

## Step 3: Plugin Marketplaces

### Description

Claude Code supports a plugin system to extend functionality. This guide uses 3 plugin marketplaces:

| Marketplace                  | Purpose                       | Source                                                          |
|------------------------------|-------------------------------|-----------------------------------------------------------------|
| **claude-plugins-official**  | Official plugins              | [GitHub](https://github.com/anthropics/claude-plugins-official) |
| **zai-coding-plugins**       | GLM-specific plugins          | Auto-configured by `@z_ai/coding-helper`                        |
| **claude-hud**               | Status bar display            | [GitHub](https://github.com/jarrodwatts/claude-hud)             |

### Add Plugin Marketplaces

```bash
# Add official plugin marketplace
claude plugin marketplace add anthropics/claude-plugins-official

# Add Claude HUD status bar
claude plugin marketplace add jarrodwatts/claude-hud

# Note: zai-coding-plugins is auto-configured by bunx @z_ai/coding-helper in Step 2

# Verify all marketplaces are added
claude plugin marketplace list
```

---

## Step 4: Install Plugins

### Description

Now install 15 plugins. Each plugin includes installation command and description.

### 4.1 GLM Plugin Marketplace (1 plugin)

> Auto-configured by `bunx @z_ai/coding-helper` in Step 2. Shown here for reference.

```bash
# glm-plan-usage - GLM Coding Plan usage query
claude plugin install glm-plan-usage@zai-coding-plugins
```

### 4.2 Official Plugin Marketplace (13 plugins)

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

### 4.3 Other Marketplaces (1 plugin)

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

## Step 5: MCP Servers

### Description

MCP (Model Context Protocol) server configuration (6 total):

**Auto-configured MCP (via GLM configuration):**
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

## Step 6: Hooks

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

## Step 7: Auxiliary Tools

### Description

Auxiliary tools were installed in previous steps, only need verification and configuration:

- **claude-hud** - Installed in Step 4.3
- **RTK** - Installed in Step 6.1

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

## Step 8: Verification

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
jq -e '.env.ENABLE_TOOL_SEARCH' ~/.claude/settings.json >/dev/null 2>&1 && echo "  ENABLE_TOOL_SEARCH set" || echo "  WARNING: ENABLE_TOOL_SEARCH missing (GLM compatibility)"
jq -e '.env.CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS' ~/.claude/settings.json >/dev/null 2>&1 && echo "  DISABLE_EXPERIMENTAL_BETAS set" || echo "  WARNING: DISABLE_EXPERIMENTAL_BETAS missing (GLM compatibility)"
jq -e '.env.CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC' ~/.claude/settings.json >/dev/null 2>&1 && echo "  DISABLE_NONESSENTIAL_TRAFFIC set" || echo "  WARNING: DISABLE_NONESSENTIAL_TRAFFIC missing (GLM compatibility)"

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

## Step 9: Troubleshooting

### 9.1 GLM Configuration Issues

**Problem: Invalid GLM API Key**
```bash
# Diagnose
curl -H "Authorization: Bearer YOUR_API_KEY" https://open.bigmodel.cn/api/anthropic

# Solution
# Re-run configuration helper
bunx @z_ai/coding-helper
```

### 9.2 Plugin Issues

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

### 9.3 MCP Issues

**Problem: MCP Servers Not Showing**
```bash
# List MCP servers
claude mcp list

# Check configuration
jq '.mcpServers' ~/.claude.json

# Restart Claude Code
```

### 9.4 Hooks Issues

**Problem: Hooks Not Working**
```bash
# Check configuration
jq '.hooks' ~/.claude/settings.json

# Check RTK
rtk --version
rtk init --show
```

### 9.5 Network Issues

**Problem: Cannot Access GitHub**
```bash
# Check network connectivity
curl -I https://github.com

# Set proxy (if needed)
export HTTPS_PROXY="http://your-proxy:port"
```

### 9.6 Complete Reset

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
# Re-execute all steps starting from Step 1
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
