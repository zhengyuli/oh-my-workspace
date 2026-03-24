# Yazi Configuration Design

**Date:** 2026-03-24
**Author:** zhengyu.li
**Status:** Draft

## Overview

Add yazi terminal file manager configuration to oh-my-workspace with developer-focused settings, Vim-style navigation, and full preview capabilities.

## Requirements

### User Requirements
- Developer-focused configuration optimized for code navigation
- Hybrid keybinding style (Vim navigation + minimal custom bindings)
- Minimal integrations (basic file operations only, no git/editor integration)
- Full preview capabilities (code, images, videos, PDFs)
- Default theme (no custom theming)
- Simple and efficient approach

### Functional Requirements
- Enter directories smoothly (not opening in editor)
- Open files with appropriate tools
- Fast navigation with Vim-style keys
- Tab management for multi-directory workflows
- Smart file sorting (natural sort, directories first)

## Design

### Package Structure

```
tool/yazi/
└── .config/
    └── yazi/
        ├── yazi.toml      # Main configuration
        └── keymap.toml    # Keybindings
```

**Location:** `tool/yazi/` following oh-my-workspace conventions
**Target:** `~/.config/yazi/` via GNU Stow
**Registration:** Add `"tool/yazi"` to `PKG_ALL` in `setup.sh`

### Configuration Files

#### yazi.toml - Main Configuration

**Manager Section:**
- Layout ratio: `[1, 4, 3]` (parent:current:preview)
- Sort by: `natural` (handles version numbers correctly)
- Directories first: `true`
- Line mode: `size` (show file sizes)
- Show symlinks: `true`
- Scroll offset: `5` lines

**Preview Section:**
- No line wrap for code
- Tab size: 2 spaces
- Image max: 600x900 pixels
- Image quality: 75 (balanced quality/cache size)
- Image filter: `lanczos3` (best quality)
- Image delay: 50ms (prevent scroll lag)

**Tasks Section:**
- Micro workers: 10 (quick operations)
- Macro workers: 25 (heavy operations)
- Image memory limit: 512MB
- No image size limit

**Opener Configuration:**
- `edit`: Open with `$EDITOR` (blocking mode)
- `open`: Open with macOS `open` command
- `extract`: Extract archives with yazi

**Open Rules:**
- Text files (`text/*`): Use `$EDITOR`
- Everything else: Use system default
- Note: Directory navigation is handled by keymap commands, not the opener system

#### keymap.toml - Keybindings

**Navigation:**
- `h`: Escape / go back to parent directory
- `l`: Enter directory or open file
- `Enter`: Enter directory or open file (explicit)

**Tab Management:**
- `T`: Create tab at current path
- `t`: Create new tab
- `1-5`: Switch to tabs 1-5

**Rationale:**
- Vim-style navigation with `h`/`l` for horizontal movement
- No Emacs-style shortcuts (minimal integration approach)
- Tab numbers for quick switching
- Yazi defaults preserved for all other operations

### Integration Points

**Brewfile:**
- Yazi already present in `pkg/homebrew/Brewfile`
- Optional dependencies for full previews:
  - Video thumbnails (ffmpegthumbnailer)
  - Archive extraction (7-zip)
  - PDF preview (poppler)

**Setup Script:**
- Add `"tool/yazi"` to `PKG_ALL` array in `setup.sh`
- Stow will create symlinks to `~/.config/yazi/`

**README:**
- Yazi already documented in File Operations section
- No changes needed to README

## Implementation Plan

### Phase 1: Create Package Structure
1. Create `tool/yazi/.config/yazi/` directory structure
2. Create `yazi.toml` with configuration
3. Create `keymap.toml` with keybindings

### Phase 2: Register Package
1. Add `"tool/yazi"` to `PKG_ALL` in `setup.sh`
2. Test stow operation: `./setup.sh install yazi`

### Phase 3: Verify Configuration
1. Open yazi and test navigation
2. Verify Enter key behavior (directories vs files)
3. Test preview capabilities (code, images, etc.)
4. Verify tab management

### Phase 4: Documentation
1. Update README if needed (already documented)
2. Commit changes with conventional commit message

## Testing Strategy

### Manual Testing
1. **Navigation:**
   - `h` goes back to parent directory
   - `l` enters directory
   - `Enter` enters directory

2. **File Opening:**
   - Text file opens in `$EDITOR`
   - Image opens with system preview
   - PDF opens with system preview

3. **Tab Management:**
   - `T` creates tab at current path
   - `1-5` switches tabs correctly

4. **Preview:**
   - Code files show syntax highlighting
   - Images show inline preview
   - Videos show thumbnails (if dependencies installed)

### Edge Cases
- Symlink handling (show target path)
- Large files (preview limits)
- Hidden files (hidden by default, `.` to toggle)

## Risks and Mitigations

### Risk: Enter key opens files in wrong application
**Mitigation:** Explicit opener configuration with `prepend_rules` ensures correct behavior

### Risk: Preview performance issues
**Mitigation:** Balanced preview settings (quality 75, 50ms delay, 512MB limit)

### Risk: Keybinding conflicts
**Mitigation:** Use `prepend_keymap` to override defaults cleanly

## Future Enhancements

Potential improvements not included in initial implementation:
- Custom theme to match Ghostty terminal
- Plugin system for advanced workflows
- Custom openers for specific file types
- Git integration (currently minimal)
- Editor integration shortcuts (currently minimal)

## References

- [Yazi Configuration Documentation](https://yazi-rs.github.io/docs/configuration/yazi/)
- [Yazi Keymap Documentation](https://yazi-rs.github.io/docs/configuration/keymap/)
- [GNU Stow Manual](https://www.gnu.org/software/stow/manual/)
- [oh-my-workspace README](../../README.md)
