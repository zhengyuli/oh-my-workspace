# CLAUDE.md

macOS dotfiles repository using GNU Stow for XDG-compliant configuration management.

## Project Scope

**This is a configuration management repository**, not a code repository.

**Contains**: Configuration files (`.toml`, `.yml`, `.conf`, `.el`, `.sh`, `.zsh`)
**Purpose**: Manage dotfiles and tool configurations across machines

## Getting Started

For new users setting up the dotfiles repository:

1. **Clone the repository:**
   ```bash
   git clone https://github.com/zhengyuli/oh-my-workspace.git
   cd oh-my-workspace
   ```

2. **Preview changes (recommended):**
   ```bash
   ./setup.sh install --dry-run --all
   ```

3. **Run full setup:**
   ```bash
   ./setup.sh install --all
   ```

4. **Verify installation:**
   ```bash
   ./setup.sh status
   ```

## Architecture

### Package Structure

Organized by category in repository root:
- `shell/` — Shell configs (zsh, starship)
- `editor/` — Text editors (vim, emacs)
- `term/` — Terminal emulators (ghostty)
- `tool/` — CLI utilities (git, lazygit, ripgrep, yazi)
- `lang/` — Language runtimes (python/uv, typescript/bun)
- `platform/` — Platform-specific configs (darwin)

Files follow GNU Stow convention: placed as they appear in `$HOME`.

**XDG Base Directory compliance**: Use `$XDG_CONFIG_HOME` (`~/.config`), `$XDG_DATA_HOME`, `$XDG_CACHE_HOME`, `$XDG_STATE_HOME` for all paths — never hardcode user-specific directories.

### Directory Structure

```
oh-my-workspace/
├── setup.sh           # Main setup script
├── CLAUDE.md
├── LICENSE
├── README.md
│
├── claude/            # Claude Code environment
│   └── setup.md       # Setup guide for Claude Code
│
├── docs/              # Documentation
│
├── shell/             # Shell configurations
│   ├── starship/      # Starship prompt config (~/.config/starship.toml)
│   └── zsh/           # Zsh config (~/.config/zsh/, ~/.zshenv)
│
├── editor/            # Text editors
│   ├── emacs/         # Emacs modular config (~/.config/emacs/)
│   └── vim/           # Vim config (~/.config/vim/)
│
├── term/              # Terminal emulators
│   └── ghostty/       # Ghostty config (~/.config/ghostty/)
│
├── tool/              # CLI utilities
│   ├── git/           # Git config (~/.config/git/)
│   ├── lazygit/       # Lazygit config (~/.config/lazygit/)
│   ├── ripgrep/       # Ripgrep config (~/.config/ripgrep/)
│   └── yazi/          # Yazi file manager (~/.config/yazi/)
│
├── lang/              # Language runtimes
│   ├── python/uv/     # uv config (~/.config/uv/)
│   └── typescript/bun/# bun config (~/.config/bun/)
│
├── platform/          # Platform-specific
│   └── darwin/        # macOS scripts (defaults.sh)
│
└── pkg/               # Package management
    └── homebrew/      # Brewfile
```

## Development Workflow

```bash
# Full setup (prerequisites + packages + symlinks)
./setup.sh install --all

# Stow specific packages
./setup.sh install zsh git vim

# Restow after changes
./setup.sh install --force zsh

# Preview changes
./setup.sh install --dry-run --all

# Check status
./setup.sh status
```

**Package Registry**: See `PKG_ALL` array in `setup.sh`

**Dependencies**: Managed via `pkg/homebrew/Brewfile`

## GLM API Configuration

This environment uses Zhipu GLM API instead of Anthropic. The following env vars in `~/.claude/settings.json` are critical for GLM compatibility — do NOT remove:
- `ENABLE_TOOL_SEARCH: "0"` — prevents tool search conflicts
- `CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS: "1"` — prevents unsupported API calls
- `CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC: "1"` — stops Anthropic telemetry

Model mapping: Haiku → glm-4.5-air, Sonnet → glm-5-turbo, Opus → glm-5.1

See `claude/setup.md` for full environment setup guide.

## Coding Conventions

Detailed conventions in `.claude/rules/`:

- `bash.md` — Bash shell scripting conventions
- `zsh.md` — Zsh-specific features and conventions
- `elisp.md` — Emacs Lisp conventions
- `config.md` — Configuration files (no extension)
- `toml.md` — TOML configuration files
- `yaml.md` — YAML configuration files

## Quick Reference

| Task              | Command                                  |
|-------------------|------------------------------------------|
| First-time setup  | See Getting Started section above        |
| Prerequisites     | `brew install stow`                      |
| Claude Code setup | See `claude/setup.md` (15 plugins, 6 MCP servers, RTK, claude-hud) |
| Full setup        | `./setup.sh install --all`               |
| Stow package      | `./setup.sh install <package>`           |
| Restow            | `./setup.sh install --force <package>`   |
| Preview           | `./setup.sh install --dry-run <package>` |
| Status            | `./setup.sh status`                      |
| Unstow            | `./setup.sh uninstall <package>`         |

## Troubleshooting

### Common Issues

**Stow conflict: file already exists**

GNU Stow refuses to create a symlink if the target already exists as a real file.

```bash
# Backup and remove conflicting file
cp ~/.zshrc ~/.zshrc.bak
rm ~/.zshrc

# Now stow succeeds
./setup.sh install zsh
```

---

For detailed coding standards, see `.claude/rules/`.

## Skill routing

When the user's request matches an available skill, invoke it via the Skill tool. The
skill has multi-step workflows, checklists, and quality gates that produce better
results than an ad-hoc answer. When in doubt, invoke the skill. A false positive is
cheaper than a false negative.

Key routing rules:
- Product ideas, "is this worth building", brainstorming → invoke /office-hours
- Strategy, scope, "think bigger", "what should we build" → invoke /plan-ceo-review
- Architecture, "does this design make sense" → invoke /plan-eng-review
- Design system, brand, "how should this look" → invoke /design-consultation
- Design review of a plan → invoke /plan-design-review
- Developer experience of a plan → invoke /plan-devex-review
- "Review everything", full review pipeline → invoke /autoplan
- Bugs, errors, "why is this broken", "wtf", "this doesn't work" → invoke /investigate
- Test the site, find bugs, "does this work" → invoke /qa (or /qa-only for report only)
- Code review, check the diff, "look at my changes" → invoke /review
- Visual polish, design audit, "this looks off" → invoke /design-review
- Developer experience audit, try onboarding → invoke /devex-review
- Ship, deploy, create a PR, "send it" → invoke /ship
- Merge + deploy + verify → invoke /land-and-deploy
- Configure deployment → invoke /setup-deploy
- Post-deploy monitoring → invoke /canary
- Update docs after shipping → invoke /document-release
- Weekly retro, "how'd we do" → invoke /retro
- Second opinion, codex review → invoke /codex
- Safety mode, careful mode, lock it down → invoke /careful or /guard
- Restrict edits to a directory → invoke /freeze or /unfreeze
- Upgrade gstack → invoke /gstack-upgrade
- Save progress, "save my work" → invoke /context-save
- Resume, restore, "where was I" → invoke /context-restore
- Security audit, OWASP, "is this secure" → invoke /cso
- Make a PDF, document, publication → invoke /make-pdf
- Launch real browser for QA → invoke /open-gstack-browser
- Import cookies for authenticated testing → invoke /setup-browser-cookies
- Performance regression, page speed, benchmarks → invoke /benchmark
- Review what gstack has learned → invoke /learn
- Tune question sensitivity → invoke /plan-tune
- Code quality dashboard → invoke /health
