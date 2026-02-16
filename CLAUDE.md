# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a comprehensive workspace configuration repository for "Oh My Workspace" - a centralized development environment setup that integrates multiple tools and configurations for streamlining development workflows. The project provides automated setup scripts and configurations for various development tools including editors, terminal environments, and programming languages.

## Development Setup Commands

### Initial Setup
- **Claude Code Setup**: `./claudecode/claude-code-setup.sh` - Installs Node.js, Claude Code, and configures API settings with ZHIPU AI integration
- **Emacs Configuration**: `./emacs/install.sh` - Installs and configures Emacs with custom settings
- **Vim Configuration**: `./vim/install.sh` - Installs and configures Vim with custom settings

### Configuration Management
- The repository uses modular design with separate directories for each tool
- All configurations are cross-platform (macOS and Ubuntu support)
- Setup scripts are designed to be idempotent and can be run safely multiple times

## Architecture Overview

### Directory Structure
```
├── claudecode/          # Claude Code setup and configuration
├── emacs/               # Emacs configuration with custom packages
├── vim/                 # Vim configuration
├── .claude/             # Claude Code settings and permissions
└── README.org           # Comprehensive documentation
```

### Key Components

#### Claude Code Integration
- **Setup Script**: Automates Node.js installation (v22), npm, and Claude Code global installation
- **Configuration**: Uses ZHIPU AI (open.bigmodel.cn) as API provider with GLM models
- **Models**: Configured with GLM-4.5-Air (Haiku), GLM-4.6 (Opus/Sonnet)
- **Permissions**: Defined in `.claude/settings.local.json` with specific allowlist for safe operations

#### Emacs Configuration
- **Package Management**: Custom package loading system with separate paths for stable and temporary packages
- **Path Structure**:
  - `custom/`: Custom Emacs Lisp settings
  - `site-packages/`: Third-party package configurations
  - `templates/`: Template files for various file types
  - `banners/`: Custom startup banner designs
- **Installation**: Automatic setup with dependency management for LSP servers and tools

#### Vim Configuration
- **Core Features**: Syntax highlighting, search enhancements, encoding detection
- **Key Settings**: Autoindent, smarttab, 4-space indentation, 256-color support
- **Cross-Platform**: Compatible with macOS and Linux environments

#### Security Configuration
- **Bash Permissions**: Limited set of allowed commands for safety
- **File Access**: Restricted to user directory paths
- **Git Operations**: Basic version control with checkout and restore capabilities

## Development Workflow

### Common Commands
- **Git**: Standard git operations with restricted permissions
- **File Management**: Read/write operations within user directory
- **Installation**: Running install scripts for various tools
- **Testing**: Validate setup with provided validation scripts

### Tool-Specific Notes
- **Claude Code**: Requires ZHIPU AI API key for authentication
- **Emacs**: Needs manual font installation after setup (`M-x all-the-icons-install-fonts`)
- **Vim**: Configuration optimized for development with modern coding features
- **Cross-Platform**: All configurations designed to work on both macOS and Linux

## Configuration Philosophy

### Modularity
- Each tool has its own configuration directory
- Independent installation scripts for each component
- Centralized documentation in README.org

### Automation
- Automated setup scripts reduce manual configuration
- Cross-platform support for consistent environments
- Idempotent installations for safe repeated use

### Security
- Restricted bash permissions for safe operation
- No hardcoded credentials or API keys
- File access limited to appropriate directories

## Important Notes

- The repository uses `.org` format for documentation (Emacs org-mode)
- All installations require appropriate system dependencies
- Claude Code integration specifically uses Chinese AI models
- Configuration files are designed for personal use but can be adapted
- Setup scripts assume standard home directory structure
