# iTerm2 Configuration

This directory contains iTerm2 profile configuration.

## Import Profile

1. Open iTerm2
2. Go to **Preferences** (`Cmd + ,`)
3. Click on the **Profiles** tab
4. Click **Other Actions** → **Import JSON Profiles...**
5. Select `iterm2_profile.json` from this directory

## Export Current Profile

To export your current iTerm2 settings:

1. Open iTerm2
2. Go to **Preferences** (`Cmd + ,`)
3. Click on the **Profiles** tab
4. Select your profile
5. Click **Other Actions** → **Save Profile as JSON...**
6. Save to this directory as `iterm2_profile.json`

## Recommended Settings

### Appearance
- Theme: Minimal
- Tab bar location: Top
- Show tab bar: Always

### Profiles → General
- Working Directory: Home directory
- Command: `/opt/homebrew/bin/zsh` (or `/bin/zsh`)

### Profiles → Colors
- Color Preset: Import from profile or choose a theme

### Profiles → Text
- Font: JetBrains Mono, Menlo, or SF Mono
- Font Size: 14-16pt
- Use thin strokes: On retina displays

### Profiles → Window
- Transparency: 10-15% (optional)
- Blur: Yes (if using transparency)
- Style: Normal or No Title Bar

### Profiles → Keys
- Key Mappings: Option Key as `Esc+`

### Profiles → Terminal
- Scrollback Lines: 10000 or Unlimited

### Profiles → Session
- Prompt before closing: Only if there are running jobs

## Dynamic Profile (Advanced)

For automatic profile updates, you can use iTerm2's dynamic profiles:

1. Create a directory: `~/Library/Application Support/iTerm2/DynamicProfiles/`
2. Place JSON profile files there
3. iTerm2 will automatically load and update them

## Shell Integration

Install iTerm2 shell integration for enhanced features:

```bash
curl -L https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh | bash
```

Then add to `~/.zshrc.local`:

```zsh
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
```

## Useful Shortcuts

| Shortcut               | Action             |
|------------------------|--------------------|
| `Cmd + D`              | Split vertically   |
| `Cmd + Shift + D`      | Split horizontally |
| `Cmd + [/]`            | Navigate tabs      |
| `Cmd + Option + Arrow` | Navigate splits    |
| `Cmd + Shift + Enter`  | Maximize pane      |
| `Cmd + ;`              | Show autocomplete  |
| `Cmd + Shift + H`      | Show paste history |
| `Cmd + Option + E`     | Expose all tabs    |
