# Emacs Configuration Structure

> This document describes the modular structure of the Emacs configuration after the 2026-02-27 restructuring.

## Directory Structure

```
emacs/
├── init.el                         # Entry point
├── lisp/
│   ├── core/                    # Core infrastructure
│   │   ├── init-funcs.el         # Utility functions + timer + validation
│   │   ├── init-base.el         # Base configuration + package management
│   │   └── init-env.el          # Environment + platform settings
│   ├── ui/                     # User interface
│   │   ├── init-theme.el        # Theme + modeline + fullscreen
│   │   ├── init-tabs.el         # Tabs + winum
│   │   ├── init-dashboard.el    # Dashboard
│   │   ├── init-fonts.el         # Font configuration
│   │   └── init-highlight.el    # Highlights + emojify + winner
│   ├── editor/                # Editor functionality
│   │   ├── init-completion.el   # Completion system
│   │   ├── init-editing.el       # Editing enhancements (merged with switch-window + text-mode)
│   │   ├── init-projects.el     # Project management
│   │   └── init-dired.el        # File management (dired)
│   ├── tools/                # External tool integration
│   │   ├── init-vc.el            # Version control (magit)
│   │   ├── init-terminal.el     # Terminal (vterm)
│   │   ├── init-ai.el            # AI assistant (claude-code-ide)
│   │   └── init-auth.el         # Security + authentication
│   └── lang/                   # Programming language support
│       ├── init-prog.el         # Programming base (renamed from init-prog-base)
│       ├── init-elisp.el        # Emacs Lisp
│       ├── init-cc.el            # C/C++
│       ├── init-python.el        # Python
│       ├── init-go.el            # Go
│       ├── init-markdown.el      # Markdown
│       ├── init-shell.el         # Shell scripts
│       ├── init-yaml.el          # YAML
│       ├── init-cmake.el         # CMake
│       └── init-dockerfile.el    # Dockerfile
```

## Module Loading Order

Modules are loaded in `init.el` in this order:

1. **init-funcs** - Foundation (no dependencies)
2. **init-base** - Core configuration (depends on init-funcs)
3. **init-env** - Environment setup (depends on init-funcs)
4. **init-fonts** - Font setup (depends on init-funcs)
5. **init-theme** - Theme setup (depends on init-funcs)
6. **init-tabs** - Tab management
7. **init-highlight** - Visual highlights
8. **init-dashboard** - Dashboard (depends on init-theme, init-tabs)
9. **init-completion** - Completion system (depends on init-funcs)
10. **init-editing** - Editing enhancements (depends on init-funcs)
11. **init-projects** - Project management
12. **init-dired** - File management (depends on init-funcs)
13. **init-vc** - Version control (depends on init-funcs)
14. **init-terminal** - Terminal (depends on init-funcs)
15. **init-ai** - AI assistant
16. **init-auth** - Authentication (depends on init-funcs)
17. **init-prog** - Programming base (depends on init-funcs)
18. Language modules (various dependencies)

## Key Changes

### From init-utilities.el

Content was distributed to:

| Target | Content |
|--------|---------|
| `init-funcs.el` | Timer management system |
| `init-base.el` | Core configuration hooks, package management, restart-emacs, aliases |
| `init-env.el` | exec-path-from-shell, mac system settings |
| `init-editing.el` | switch-window, text-scale keybindings |
| `init-highlight.el` | winner keybindings |
| `init-prog.el` | prog-before-save-hook |

### From init-ui.el

Content was distributed to:

| Target | Content |
|--------|---------|
| `init-theme.el` | Random banner
 fullscreen hook
 doom-themes
 doom-modeline
 smooth scrolling |
| `init-tabs.el` | nerd-icons
 centaur-tabs
 winum |
| `init-dashboard.el` | dashboard configuration |

### From init-text.el

Content was merged into `init-editing.el` (text-mode hooks)

### Renamed Files

| Old | New |
|-----|-----|
| `init-functions.el` | `core/init-funcs.el` |
| `init-prog-base.el` | `lang/init-prog.el` |

### Deleted Files

- `init-functions.el` (replaced by core/init-funcs.el)
- `init-utilities.el` (content distributed to multiple modules)
- `init-ui.el` (content distributed to ui/ modules)
- `init-text.el` (merged into editor/init-editing.el)

## Verification

To verify the configuration loads correctly:

1. Start Emacs: `emacs --debug-init`
2. Run validation: `M-x config-dependency-validate`
3. Check module loading: `M-x describe-feature RET init-funcs`
