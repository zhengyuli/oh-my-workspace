# Configuration Rules Documentation

## Overview

This directory contains modular rules for Claude Code assistance in the oh-my-workspace project.

**Loading Strategy:**
- **Universal + Workflow rules** (2 files): Auto-loaded for all tasks
- **Language-specific rules** (`lang/` directory): Conditionally loaded based on file type

## Rule Categories

### Universal Rules (auto-loaded)

These rules define core standards that apply across all file types:

- **`coding-style.md`** - Universal coding standards including line length (80 chars), mandatory file headers, documentation requirements, naming conventions, and code quality principles
- **`patterns.md`** - Design patterns and anti-patterns including immutability principle, file organization, modular configuration patterns

### Language-Specific Rules (conditionally loaded)

Located in the `lang/` subdirectory, these rules extend universal standards with language-specific features:

| Rule File | Globs | Description |
|-----------|-------|-------------|
| `lang/bash.md` | `**/*.sh`, `**/*.bash` | Bash shell scripting conventions |
| `lang/zsh.md` | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, `**/zprofile`, `**/zshenv`, `**/zlogin` | Zsh shell scripting conventions |
| `lang/elisp.md` | `**/*.el` | Emacs Lisp conventions |
| `lang/config.md` | `**/config`, `**/*.conf`, `**/*.cfg`, `**/rc`, `**/.gitconfig`, `**/git/config` | Generic config files |
| `lang/toml.md` | `**/*.toml` | TOML configuration files |
| `lang/yaml.md` | `**/*.yml`, `**/*.yaml` | YAML configuration files |

**Unified Structure:** All lang rules follow a consistent 8-section template:
1. File Header
2. Delimiter Hierarchy
3. Code Patterns (with Comments subsection)
4. Language-Specific Patterns
5. Anti-Patterns
6. References
7. Validation

See `lang/_template.md` for the complete template structure.

## Conditional Loading System

### How It Works

Language-specific rules use YAML frontmatter with `globs` field:

```yaml
---
globs:
  - "**/*.py"
  - "lang/python/**"
---
```

**Behavior:**
- Universal rules are always loaded
- Language rules load only when editing matching files
- Multiple language rules can apply (e.g., `.sh` files load bash.md)

### Glob Pattern Syntax

| Pattern | Matches | Example |
|---------|---------|---------|
| `**/*.py` | All `.py` files at any depth | `script.py`, `utils/helper.py` |
| `emacs/**` | All files under directory | `emacs/init.el` |
| `setup.sh` | Specific file | `setup.sh` |

**Notes:**
- Patterns are case-sensitive
- Multiple matching rules are all loaded (additive)

## Usage Examples

### Editing Python Files
**Auto-loaded:** All 2 universal rules

### Editing Shell Scripts
**Auto-loaded:** All 2 universal rules
**Conditionally loaded:** `lang/bash.md`

### Editing Zsh Files
**Auto-loaded:** All 2 universal rules
**Conditionally loaded:** `lang/zsh.md`

## Rule Dependencies

**Key Dependencies:**
- `coding-style.md` references `patterns.md`
- `lang/*.md` files extend `coding-style.md` with language-specific patterns

## Adding New Rules

### For New Languages

1. Create new file in `lang/` directory: `lang/newlang.md`
2. Add frontmatter with appropriate globs:
   ```yaml
   ---
   globs:
     - "**/*.newlang"
     - "newlang/**"
   ---
   ```
3. Follow the unified 8-section template (see `lang/_template.md`)
4. Extend universal rules with language-specific conventions

### For New Workflow Patterns

1. Create new rule file in rules root (no globs needed - will auto-load)
2. Follow existing documentation patterns
3. Cross-reference related rules

## Rule Documentation Standards

Each rule file should include:

1. **Clear title and purpose** - What the rule covers
2. **References** - Links to authoritative style guides
3. **Examples** - Concrete code examples showing good vs bad patterns
4. **Rationale** - Why the rule exists
5. **Cross-references** - Links to related rules

## Maintenance

### When to Update Rules

- Adding new programming languages or tools
- Discovering new best practices
- Addressing recurring issues in AI-generated code
- Updating to newer versions of style guides

## Related Documentation

- **Project root**: `/CLAUDE.md` - High-level project guide
- **Setup script**: `/setup.sh` - Installation and stow operations
- **Main README**: `/README.md` - Project overview and features
