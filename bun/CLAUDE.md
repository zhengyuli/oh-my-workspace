# CLAUDE.md - Bun Configuration

This file provides guidance for Claude Code when working with the Bun configuration in this directory.

## Project Overview

Bun is the **JavaScript/TypeScript runtime and package manager** used in this repository, replacing node, npm, yarn, pnpm, and fnm.

## Directory Structure

```
bun/.config/bun/
└── .bunfig.toml    # Global Bun configuration (symlinked to ~/.config/bun/.bunfig.toml)
```

## Quick Start

### Setup

```bash
# Stow the bun package
stow bun

# Verify bun is installed
bun --version
```

### Quick Validation

```bash
# Verify config is linked
ls -la ~/.config/bun/.bunfig.toml

# Test bun works
bun --version
```

## Architecture

### XDG Paths

Bun natively supports XDG Base Directory:

| Path | Purpose |
|------|---------|
| `$XDG_CONFIG_HOME/bun/` | Configuration |
| `$XDG_CACHE_HOME/bun/` | Cache |
| `$XDG_DATA_HOME/bun/` | Data (installed packages) |

### Tool Installation Convention

Bun is installed via Homebrew (see `homebrew/Brewfile`). LSP servers and development tools are installed via `bun install -g`:

```bash
# JS/TS ecosystem
bun install -g typescript-language-server typescript
bun install -g vscode-langservers-extracted
bun install -g bash-language-server
bun install -g @tailwindcss/language-server
bun install -g prettier
```

## Coding Standards

### Configuration File Standards

The `.bunfig.toml` file follows the [Root CLAUDE.md - Configuration Comment Standards](../CLAUDE.md#configuration-comment-standards).

Header example:
```toml
# .bunfig.toml
# =============================================================================
# Bun Global Configuration
#
# Location: ~/.config/bun/.bunfig.toml
# XDG: Native support
# =============================================================================
```

### Comment Character

TOML uses `#` for comments:
```toml
# This is a comment
key = "value"
```

## Code Quality

### Validation Commands

```bash
# Verify bun config is valid
bun config

# Check bun version
bun --version
```

### Compliance Checklist

- [ ] Header follows configuration comment standards
- [ ] Time-stamp present
- [ ] Comments use `#` prefix

## Best Practices

### 1. Use Bun for JS/TS

Prefer `bun` over `npm`, `yarn`, or `pnpm`:

```bash
# ✅ CORRECT
bun install
bun run dev

# ❌ AVOID
npm install
yarn dev
```

### 2. Global Tools via Bun

Install global JS/TS tools with bun:
```bash
bun install -g typescript-language-server
```

### 3. XDG Compliance

Bun respects XDG paths natively - no configuration needed.

## Quick Reference Card

### Essential Rules

1. **Package manager**: Use `bun` for all JS/TS operations
2. **Global tools**: Install via `bun install -g`
3. **Comment character**: Use `#` for comments
4. **XDG**: Native support, no configuration needed

### Pre-Commit Checklist

- [ ] Config file has standard header
- [ ] Time-stamp present
- [ ] TOML syntax valid
