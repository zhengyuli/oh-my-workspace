# Claude Code Environment Setup Guide

> **📖 For Human Readers**: This is a complete configuration guide documenting how to set up a Claude Code environment from scratch. Each step includes explanations, commands, and verification methods.
>
> **🤖 For Claude Code**: This is an executable configuration document. Please execute the command blocks in each Phase sequentially. Refer to the Troubleshooting section if you encounter errors.

## 📋 Configuration Overview

This guide will configure the following components:

| Component           | Count | Purpose                                                        |
|---------------------|-------|----------------------------------------------------------------|
| **GLM Model**       | 1     | Use Zhipu GLM to replace Anthropic Claude API                  |
| **Plugin Markets**  | 5     | Official + community plugin sources                            |
| **Plugins**         | 19    | Development tools, MCP integration, auxiliary features         |
| **MCP Servers**     | 4     | Memory management, search, browser automation, advanced search |
| **Hooks**           | 2     | Token optimization, command rewriting                          |
| **Auxiliary Tools** | 2     | RTK (token savings), claude-hud (status bar)                   |

## 🎯 Execution Order

**⚠️ Important: Must execute in order, do not skip or reorder**

```
Step 1: Prerequisites Check    → Verify system environment
Step 2: GLM Configuration      → Configure GLM API
Step 3: Plugin Markets         → Add plugin sources
Step 4: Install Plugins        → Install 19 plugins
Step 5: MCP Servers            → Configure MCP servers
Step 6: Hooks                  → Set up automation hooks
Step 7: Auxiliary Tools        → Install auxiliary tools
Step 8: Verification           → Verify all configurations
Step 9: Troubleshooting        → Troubleshoot issues
```

## 📚 Official Reference Resources

- **Claude Code**: https://docs.anthropic.com/en/docs/claude-code
- **Claude Plugins Official**: https://github.com/anthropics/claude-plugins-official
- **GLM API Documentation**: https://open.bigmodel.cn/dev/api
- **Superpowers Marketplace**: https://github.com/obra/superpowers-marketplace
- **claude-mem**: https://github.com/thedotmack/claude-mem
- **claude-hud**: https://github.com/jarrodwatts/claude-hud
- **RTK**: https://github.com/rtk-ai/rtk

---

## Step 1: Prerequisites Check

### 📝 Description

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

### 🔍 Verification Commands

**Execute the following commands to check the environment:**

```bash
# Check operating system
echo "=== Operating System ==="
uname -s  # Should show Darwin (macOS)

# Check macOS version
sw_vers -productVersion  # Needs 13.0 or higher

# Check required tools (installed via Homebrew)
echo -e "\n=== Required Tools (from Homebrew) ==="
command -v git >/dev/null 2>&1 && echo "✓ git: $(git --version | awk '{print $3}')" || echo "✗ git: NOT INSTALLED"
command -v jq >/dev/null 2>&1 && echo "✓ jq: $(jq --version)" || echo "✗ jq: NOT INSTALLED"
command -v curl >/dev/null 2>&1 && echo "✓ curl: $(curl --version | head -1 | awk '{print $2}')" || echo "✗ curl: NOT INSTALLED"
command -v bun >/dev/null 2>&1 && echo "✓ bun: $(bun --version)" || echo "✗ bun: NOT INSTALLED"
command -v uv >/dev/null 2>&1 && echo "✓ uv: $(uv --version | head -1)" || echo "✗ uv: NOT INSTALLED"

# Check claude CLI (needs separate installation)
echo -e "\n=== Claude Code CLI ==="
command -v claude >/dev/null 2>&1 && echo "✓ claude: $(claude --version)" || echo "✗ claude: NOT INSTALLED (will install in next step)"

# Check network connectivity
echo -e "\n=== Network ==="
curl -s --max-time 5 https://api.github.com >/dev/null 2>&1 && echo "✓ GitHub API accessible" || echo "✗ Cannot reach GitHub API"
```

### ✅ Expected Results

- ✓ git, jq, curl, bun, uv (from Homebrew, should be installed)
- ✗ claude (needs installation, see next step)

If Homebrew tools are missing, run:
```bash
brew bundle --file=/path/to/oh-my-workspace/pkg/homebrew/Brewfile
```

### 📦 Install Claude CLI (Official Recommended Method)

```bash
# macOS native installation (zero dependencies)
curl -fsSL https://claude.ai/install.sh | bash

# Verify installation
claude --version
```

**Installation Notes:**
- ✅ Official recommended method
- ✅ Zero dependencies, no npm/bun required
- ✅ Cross-platform support
- ✅ Installation time < 2 minutes

---

## Step 2: GLM Configuration

### 📝 Description

Configure Claude Code to use Zhipu GLM model instead of Anthropic API.
Use the official Zhipu interactive configuration tool to automatically complete:
- GLM API configuration (URL, Token)
- Model mapping (GLM-4.5-air, GLM-4.7, GLM-5)
- MCP server configuration (vision, search, web reader)
- Environment variable settings

### 🔑 Prerequisite: Obtain GLM API Key

1. Visit [Zhipu Open Platform](https://open.bigmodel.cn/)
2. Click "Register/Login" in the top right corner
3. Go to [API Keys page](https://open.bigmodel.cn/user-center/apikeys)
4. Create a new API Key and save it

**Reference Documentation:** 
- [GLM API Official Documentation](https://open.bigmodel.cn/dev/api)
- [Claude Code Configuration Guide](https://docs.bigmodel.cn/cn/guide/develop/claude)

### ⚙️ Configuration Command

```bash
# Run configuration helper using bunx
bunx @z_ai/coding-helper

# Follow the interactive prompts to complete configuration:
# 1. Select interface language
# 2. Enter GLM API Key
# 3. Select coding plan
# 4. Choose MCP servers to install (vision, search, web reader)
```

### 📊 Model Mapping

Claude Code internal models correspond to GLM models:

| Claude Model | GLM Model     | Purpose                                 |
|--------------|---------------|-----------------------------------------|
| Haiku        | `glm-4.5-air` | Fast response, lightweight tasks        |
| Sonnet       | `glm-4.7`     | Balanced performance, daily development |
| Opus         | `glm-5`       | Complex tasks, high quality output      |

**Note:** GLM-5 usage is calculated as "3x during peak hours, 2x during off-peak hours", recommended for complex tasks.

### ✅ Verify Configuration

```bash
# Check configuration file
echo "=== Claude Code Settings ==="
jq '.env | {
  ANTHROPIC_BASE_URL,
  ANTHROPIC_DEFAULT_HAIKU_MODEL,
  ANTHROPIC_DEFAULT_SONNET_MODEL,
  ANTHROPIC_DEFAULT_OPUS_MODEL
}' "$HOME/.claude/settings.json"

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

## Step 3: Plugin Markets

### 📝 Description

Claude Code supports a plugin system to extend functionality. This guide uses 5 plugin markets:

| Market                      | Plugin Count | Purpose                       | Source                                                          |
|-----------------------------|--------------|-------------------------------|-----------------------------------------------------------------|
| **claude-plugins-official** | 11           | Official plugins              | [GitHub](https://github.com/anthropics/claude-plugins-official) |
| **superpowers-marketplace** | 5            | Superpowers plugin collection | [GitHub](https://github.com/obra/superpowers-marketplace)       |
| **thedotmack**              | 1            | Memory management             | [GitHub](https://github.com/thedotmack/claude-mem)              |
| **marketingskills**         | 1            | Marketing skills              | [GitHub](https://github.com/coreyhaines31/marketingskills)      |
| **claude-hud**              | 1            | Status bar display            | [GitHub](https://github.com/jarrodwatts/claude-hud)             |

### 🔧 Add Plugin Markets

```bash
# Add official plugin market
claude /plugin marketplace add anthropics/claude-plugins-official

# Add Superpowers market
claude /plugin marketplace add obra/superpowers-marketplace

# Add claude-mem memory management
claude /plugin marketplace add thedotmack/claude-mem

# Add Marketing Skills market
claude /plugin marketplace add coreyhaines31/marketingskills

# Add Claude HUD status bar
claude /plugin marketplace add jarrodwatts/claude-hud

# Verify all markets are added
claude /plugin marketplace list
```

---

## Step 4: Install Plugins

### 📝 Description

Now install 19 plugins. Each plugin includes installation command and description.

### 📦 4.1 Official Plugin Market (11 plugins)

```bash
# playwright - Browser automation
claude /plugin install playwright@claude-plugins-official

# context7 - Document query
claude /plugin install context7@claude-plugins-official

# ralph-loop - Loop execution
claude /plugin install ralph-loop@claude-plugins-official

# commit-commands - Git commit commands
claude /plugin install commit-commands@claude-plugins-official

# claude-md-management - CLAUDE.md management
claude /plugin install claude-md-management@claude-plugins-official

# plugin-dev - Plugin development tools
claude /plugin install plugin-dev@claude-plugins-official

# hookify - Hook management
claude /plugin install hookify@claude-plugins-official

# skill-creator - Skill creation tool
claude /plugin install skill-creator@claude-plugins-official

# code-review - Code review
claude /plugin install code-review@claude-plugins-official

# feature-dev - Feature development
claude /plugin install feature-dev@claude-plugins-official

# code-simplifier - Code simplification
claude /plugin install code-simplifier@claude-plugins-official

# frontend-design - Frontend design
claude /plugin install frontend-design@claude-plugins-official
```

### 📦 4.2 Superpowers Marketplace (5 plugins)

```bash
# superpowers - Core skill library
claude /plugin install superpowers@superpowers-marketplace

# elements-of-style - Writing guidance
claude /plugin install elements-of-style@superpowers-marketplace

# superpowers-developing-for-claude-code - Claude Code development
claude /plugin install superpowers-developing-for-claude-code@superpowers-marketplace

# claude-session-driver - Session driver
claude /plugin install claude-session-driver@superpowers-marketplace

# double-shot-latte - Double-shot functionality
claude /plugin install double-shot-latte@superpowers-marketplace
```

### 📦 4.3 Other Markets (3 plugins)

```bash
# claude-mem - Memory management
claude /plugin install claude-mem@thedotmack

# marketing-skills - Marketing skills
claude /plugin install marketing-skills@marketingskills

# claude-hud - Status bar display
claude /plugin install claude-hud@claude-hud
```

### ✅ Verify All Plugins

```bash
# List all installed plugins
claude /plugin list

# Should see 19 plugins
```

---

## Step 5: MCP Servers

### 📝 Description

MCP (Model Context Protocol) server configuration:

**Auto-configured MCP (via GLM configuration):**
- `web-reader` - Web content reader
- `web-search-prime` - Web search
- `zai-mcp-server` - Multimodal content analysis

**Independent configuration MCP (global):**
- `tavily-search` - Advanced web search (optional)

**Plugin auto-configured MCP:**
- `claude-mem` (mcp-search) - Persistent memory
- `context7` - Library documentation query
- `playwright` - Browser automation

### 📦 5.1 Configure tavily-search (Optional)

```bash
# 1. Obtain Tavily API Key
# Visit: https://tavily.com/
# Register account and get API Key

# 2. Add global MCP configuration
jq '.mcpServers += {
  "tavily-search": {
    "type": "http",
    "url": "https://mcp.tavily.com/mcp/?tavilyApiKey=YOUR_TAVILY_API_KEY"
  }
}' ~/.claude.json > /tmp/claude.json.tmp && mv /tmp/claude.json.tmp ~/.claude.json

# 3. Verify configuration
jq '.mcpServers | keys' ~/.claude.json
```

**Reference:** [Tavily Official Site](https://tavily.com/)

### ✅ Verify All MCP

```bash
# List all MCP servers
claude mcp list

# Verify GLM MCP configuration
jq '.mcpServers | keys' ~/.claude.json

# Verify claude-mem Worker
curl -s http://localhost:37777/health | jq .
```

---

## Step 6: Hooks

### 📝 Description

Hooks are Claude Code automation scripts:

| Hook    | Trigger           | Purpose                   | Source                                      |
|---------|-------------------|---------------------------|---------------------------------------------|
| **RTK** | PreToolUse (Bash) | Token optimization 60-90% | [rtk-ai/rtk](https://github.com/rtk-ai/rtk) |

### 📦 6.1 Install RTK

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

### ✅ Verify Hooks

```bash
# Check hooks configuration
jq '.hooks' ~/.claude/settings.json

# Check RTK status
rtk --version
rtk init --show
```

---

## Step 7: Auxiliary Tools

### 📝 Description

Auxiliary tools were installed in previous steps, only need verification and configuration:

- **claude-hud** - Installed in Step 4.3
- **RTK** - Installed in Step 6.1

### 📦 7.1 Configure claude-hud Statusline

```bash
# Configure statusline (interactive)
claude /claude-hud:setup

# Optional: Configure HUD display options
claude /claude-hud:configure
```

**HUD Display Content:**
- Project path, Git status
- Context usage, Token usage
- Tool activity, Agent status, Todo progress

### ✅ Verify All Tools

```bash
# 1. Verify claude-hud
claude /plugin list | grep claude-hud
jq '.statusLine' ~/.claude/settings.json

# 2. Verify RTK
rtk --version
rtk init --show

# 3. Verify Plugin count
claude /plugin list | wc -l
echo "Expected: 19 plugins"
```

---

## Step 8: Verification

### 📝 Description

After completing all configurations, perform comprehensive verification.

### ✅ Complete Verification Script

```bash
#!/bin/bash

echo "========================================="
echo "Claude Code Environment Verification"
echo "========================================="

# 1. GLM Configuration Check
echo -e "\n[1/7] GLM Configuration Check"
jq -e '.env.ANTHROPIC_BASE_URL' ~/.claude/settings.json >/dev/null 2>&1 && echo "✓ GLM configuration exists" || echo "✗ GLM configuration missing"

# 2. Plugin Count Check
echo -e "\n[2/7] Plugin Count Check"
PLUGIN_COUNT=$(claude /plugin list 2>/dev/null | wc -l | xargs)
[[ "$PLUGIN_COUNT" -ge 18 ]] && echo "✓ Plugin count: $PLUGIN_COUNT" || echo "⚠ Low plugin count: $PLUGIN_COUNT"

# 3. MCP Check
echo -e "\n[3/7] MCP Check"
MCP_COUNT=$(jq '.mcpServers | length' ~/.claude.json 2>/dev/null)
[[ "$MCP_COUNT" -ge 3 ]] && echo "✓ MCP count: $MCP_COUNT" || echo "⚠ Low MCP count: $MCP_COUNT"

# 4. Hooks Check
echo -e "\n[4/7] Hooks Check"
jq -e '.hooks.PreToolUse' ~/.claude/settings.json >/dev/null 2>&1 && echo "✓ Hooks configured" || echo "⚠ Hooks not configured"

# 5. RTK Check
echo -e "\n[5/7] RTK Check"
command -v rtk >/dev/null 2>&1 && echo "✓ RTK installed: $(rtk --version 2>&1 | head -1)" || echo "✗ RTK not installed"

# 6. claude-mem Worker Check
echo -e "\n[7/7] claude-mem Worker Check"
curl -s http://localhost:37777/health >/dev/null 2>&1 && echo "✓ claude-mem Worker running" || echo "⚠ claude-mem Worker not running"

# 8. Configuration File Format Check
echo -e "\n[8/7] Configuration File Format Check"
jq empty ~/.claude/settings.json 2>/dev/null && echo "✓ settings.json" || echo "✗ settings.json"
jq empty ~/.claude.json 2>/dev/null && echo "✓ .claude.json" || echo "✗ .claude.json"

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

### 🔧 9.1 GLM Configuration Issues

**Problem: Invalid GLM API Key**
```bash
# Diagnose
curl -H "Authorization: Bearer YOUR_API_KEY" https://open.bigmodel.cn/api/anthropic

# Solution
# Re-run configuration helper
bunx @z_ai/coding-helper
```

### 🔧 9.2 Plugin Issues

**Problem: Plugin Installation Failed**
```bash
# Check if market is added
claude /plugin marketplace list

# Re-add market
claude /plugin marketplace add anthropics/claude-plugins-official

# Manually clone market
cd ~/.claude/plugins/marketplaces
git clone https://github.com/anthropics/claude-plugins-official.git
```

**Problem: Plugin Not Loading**
```bash
# Restart Claude Code
# Ctrl+C to exit, then run claude again

# Reinstall Plugin
claude /plugin install plugin-name@marketplace
```

### 🔧 9.3 MCP Issues

**Problem: MCP Servers Not Showing**
```bash
# List MCP servers
claude mcp list

# Check configuration
jq '.mcpServers' ~/.claude.json

# Restart Claude Code
```

**Problem: claude-mem Worker Not Running**
```bash
# Manually start Worker
cd ~/.claude/plugins/cache/thedotmack/claude-mem/10.5.4
bun run start

# Check logs
tail -f ~/.claude-mem/logs/worker.log
```

### 🔧 9.4 Hooks Issues

**Problem: Hooks Not Working**
```bash
# Check configuration
jq '.hooks' ~/.claude/settings.json

# Check RTK
rtk --version
rtk init --show
```

### 🔧 9.5 Network Issues

**Problem: Cannot Access GitHub**
```bash
# Check network connectivity
curl -I https://github.com

# Set proxy (if needed)
export HTTPS_PROXY="http://your-proxy:port"
```

### 🔧 9.6 Complete Reset

**Problem: All Configuration Failed**
```bash
# ⚠️ Warning: This will delete all configurations

# 1. Backup important files
cp ~/.claude/settings.json ~/settings.json.backup
cp ~/.claude.json ~/claude.json.backup

# 2. Delete configuration directories
rm -rf ~/.claude/
rm -rf ~/.claude-mem/
rm -rf ~/.config/rtk/

# 3. Restart configuration
# Re-execute all steps starting from Step 1
```

---

**Configuration Complete!**

Your Claude Code environment is now fully configured with:
- ✅ GLM model configuration
- ✅ 19 plugins
- ✅ 4+ MCP servers
- ✅ RTK token optimization
- ✅ claude-hud status bar

Enjoy using Claude Code! 🚀
