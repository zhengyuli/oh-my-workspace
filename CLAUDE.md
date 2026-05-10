# CLAUDE.md

macOS dotfiles repository with a built-in symlink engine for XDG-compliant configuration management.

## Project Scope

**This is a configuration management repository**, not a code repository.

**Contains**: Configuration files (`.toml`, `.yml`, `.conf`, `.el`, `.sh`, `.zsh`), setup scripts, and a BATS test suite
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
- `shell/` — Shell configs (zsh)
- `editor/` — Text editors (vim, emacs)
- `terminal/` — Terminal emulators (ghostty)
- `tool/` — CLI utilities (git, bat, tmux, lazygit, ripgrep, starship, yazi)
- `prog-lang/` — Language runtimes (python/uv, typescript/bun)
- `platform/` — Platform-specific configs (darwin)

Files map directly to their targets under `$XDG_CONFIG_HOME` via the built-in symlink engine.

**XDG Base Directory compliance**: Use `$XDG_CONFIG_HOME` (`~/.config`), `$XDG_DATA_HOME`, `$XDG_CACHE_HOME`, `$XDG_STATE_HOME` for all paths — never hardcode user-specific directories.

### Directory Structure

```
oh-my-workspace/
├── setup.sh           # Main setup script
├── CLAUDE.md
├── LICENSE
├── README.md
│
├── agent/          # AI coding agent configs (Claude Code, etc.)
│   └── claude/        # Claude Code setup (pre-setup.sh, setup.md)
│
├── tests/             # BATS test suite (333 tests)
│   ├── zsh-helper.bash # Shared zsh test utilities
│   ├── mocks/         # Mock dirs (setup/, pre-setup/, zsh/)
│   └── *.bats        # Test files (16 files)
│
├── shell/             # Shell configurations
│   └── zsh/           # Zsh config (~/.config/zsh/, ~/.zshenv)
│
├── editor/            # Text editors
│   ├── emacs/         # Emacs modular config (~/.config/emacs/)
│   └── vim/           # Vim config (~/.config/vim/)
│
├── terminal/          # Terminal emulators
│   └── ghostty/       # Ghostty config (~/.config/ghostty/)
│
├── tool/              # CLI utilities
│   ├── bat/           # Bat config (~/.config/bat/)
│   ├── git/           # Git config (~/.config/git/)
│   ├── lazygit/       # Lazygit config (~/.config/lazygit/)
│   ├── ripgrep/       # Ripgrep config (~/.config/ripgrep/)
│   ├── starship/      # Starship prompt (~/.config/starship.toml)
│   ├── tmux/          # Tmux config (~/.config/tmux/)
│   └── yazi/          # Yazi file manager (~/.config/yazi/)
│
├── prog-lang/         # Language runtimes
│   ├── python/uv/     # uv config (~/.config/uv/)
│   └── typescript/bun/# bun config (~/.config/.bunfig.toml)
│
├── platform/          # Platform-specific
│   └── darwin/        # macOS scripts (defaults.sh)
│
└── pkg-manager/       # Package management
    └── homebrew/      # Brewfile
```

## Development Workflow

```bash
# Full setup (prerequisites + packages + symlinks)
./setup.sh install --all

# Link specific packages
./setup.sh install zsh git vim

# Relink after changes
./setup.sh install --force zsh

# Preview changes
./setup.sh install --dry-run --all

# Check status
./setup.sh status
```

**Package Registry**: See `PKG_ALL` array in `setup.sh`

**Dependencies**: Managed via `pkg-manager/homebrew/Brewfile`

## Testing

**Test framework**: [BATS](https://github.com/bats-core/bats-core) (Bash Automated Testing System)

```bash
# Run full suite (333 tests)
bats tests/

# Run specific module
bats tests/zsh-00-env.bats

# Run with verbose output
bats --verbose-run tests/
```

**Test architecture**: BATS orchestrates; zsh modules run via `zsh -c` subprocesses for native zsh feature support. All tests use isolated `$HOME`, mock PATH, and no network access.

**Key files**:
- `tests/zsh-helper.bash` — shared utilities (`run_zsh`, `setup_zsh_env`, `teardown_zsh_env`)
- `tests/mocks/zsh/` — mock scripts for zsh tests (brew, starship, fzf, uv, zoxide, defaults, etc.)
- `tests/mocks/setup/` — mock scripts for setup.sh tests (curl, uname, etc.)
- `tests/mocks/pre-setup/` — mock scripts for pre-setup.sh tests (bunx, claude, jq, sw_vers)

**Adding tests for a new zsh module**:
1. Create `tests/zsh-<NN>-<name>.bats`
2. Use `load zsh-helper` + `setup() { setup_zsh_env; }` + `teardown() { teardown_zsh_env; }`
3. Test with `run_zsh "$MODULE" '<zsh expression>'` — gives `$status` and `$output`
4. Add mock scripts to `tests/mocks/zsh/` if the module calls external tools
5. Run with `bats tests/zsh-<NN>-<name>.bats`

**Adding tests for bash scripts** (setup.sh, defaults.sh):
1. Add source guard (`if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then main "$@"; fi`)
2. Source the script in `setup()`, test individual functions with `run <function_name>`
3. Mock external commands in `tests/mocks/setup/`

**Test coverage**: 333 tests across 16 files covering setup.sh, pre-setup.sh, all zsh conf.d modules (00-70), autoloaded functions, darwin defaults, and config file validation.

## Validation

```bash
# Bash syntax check
bash -n setup.sh
bash -n platform/darwin/defaults.sh

# Bash lint (POSIX-compatible scripts only)
shellcheck setup.sh agent/claude/pre-setup.sh platform/darwin/defaults.sh

# Zsh syntax check (shellcheck has poor zsh support)
zsh -n shell/zsh/conf.d/*.zsh

# TOML validation (Python 3.11+)
python3 -c "import tomllib; tomllib.load(open('file.toml','rb'))"

# Tool-specific
ghostty +validate-config
git config --list --show-origin
```

## GLM API Configuration

This environment uses Zhipu GLM API instead of Anthropic. The following env vars in `~/.claude/settings.json` are critical for GLM compatibility — do NOT remove:
- `ENABLE_TOOL_SEARCH: "0"` — prevents tool search conflicts
- `CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS: "1"` — prevents unsupported API calls
- `CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC: "1"` — stops Anthropic telemetry

Model mapping: Haiku → glm-4.5-air, Sonnet → glm-5-turbo, Opus → glm-5.1

See `agent/claude/setup.md` for full environment setup guide.

## Coding Conventions

Detailed conventions in `.claude/rules/`:

- `bash.md` — Bash shell scripting conventions
- `zsh.md` — Zsh-specific features and conventions
- `elisp.md` — Emacs Lisp conventions
- `config.md` — Configuration files (no extension)
- `toml.md` — TOML configuration files
- `yaml.md` — YAML configuration files
- `vimrc.md` — Vim script conventions

## Health Stack

- shell: shellcheck setup.sh platform/darwin/defaults.sh agent/claude/pre-setup.sh
- test: bats tests/
- bash-syntax: bash -n setup.sh platform/darwin/defaults.sh agent/claude/pre-setup.sh
- zsh-syntax: zsh -n shell/zsh/conf.d/*.zsh

## Quick Reference

| Task              | Command                                  |
|-------------------|------------------------------------------|
| First-time setup  | See Getting Started section above        |
| Prerequisites     | Xcode CLI Tools + Homebrew               |
| Claude Code setup | See `agent/claude/setup.md` (8 plugins, 6 MCP servers, 30+ gstack skills, RTK, claude-hud) |
| Full setup        | `./setup.sh install --all`               |
| Link package      | `./setup.sh install <package>`           |
| Relink            | `./setup.sh install --force <package>`   |
| Preview           | `./setup.sh install --dry-run <package>` |
| Status            | `./setup.sh status`                      |
| Unlink            | `./setup.sh uninstall <package>`         |
| Run all tests     | `bats tests/`                            |
| Run one test file | `bats tests/<file>.bats`                 |

## Troubleshooting

### Common Issues

**Link conflict: file already exists**

The symlink engine refuses to overwrite an existing real file. Use `--force` to back it up automatically, or remove it manually:

```bash
# Option 1: force relink (backs up to *.pre-link-backup)
./setup.sh install --force zsh

# Option 2: manual backup and remove
cp ~/.zshrc ~/.zshrc.bak
rm ~/.zshrc
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
