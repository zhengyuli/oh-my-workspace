# .zprofile
# Time-stamp: <2026-03-17 00:00:00 Monday by zhengyu.li>
# =============================================================================
# Login Shell Initialization
#
# Loaded by: Login shells only (first terminal open, SSH login)
# Load order: Second (after .zshenv, before .zshrc)
#
# Responsibilities:
#   1. Source conf.d fragments safe for non-interactive contexts
#   2. Initialize SSH agent with macOS Keychain (once per login session)
#
# Do NOT add: plugins, completion, prompt, aliases, keybindings
#             → Put these in .zshrc (interactive only)
# =============================================================================

# -----------------------------------------------------------------------------
# Core Configuration Fragments
# -----------------------------------------------------------------------------
# These modules are safe for non-interactive contexts and should be available
# to all login shells (including scripts run via 'ssh host command').

# Environment variables (EDITOR, PAGER, tool XDG paths, Homebrew settings, etc.)
source "$ZDOTDIR/conf.d/00-env.zsh"

# PATH / FPATH / manpath management
source "$ZDOTDIR/conf.d/05-path.zsh"

# -----------------------------------------------------------------------------
# SSH Agent -- macOS Keychain Integration
# -----------------------------------------------------------------------------
# Add all SSH private keys to macOS Keychain once per login session.
# Supports multiple keys (id_ed25519, id_rsa, etc.)
#
# Prerequisites:
#   1. Generate key: ssh-keygen -t ed25519 -C "your@email.com"
#   2. Add to Keychain: ssh-add --apple-use-keychain ~/.ssh/id_ed25519
#   3. Add public key to GitHub/GitLab
#
# Alternative (simpler):
#   Use ~/.ssh/config with UseKeychain yes instead of this script.
#   See: https://docs.github.com/en/authentication/connecting-to-github-with-ssh
# -----------------------------------------------------------------------------
if [[ "$OSTYPE" == darwin* ]] && [[ -d ~/.ssh ]]; then
  # Ensure ssh-agent is running (macOS usually starts it via launchd)
  if [[ -z "$SSH_AGENT_PID" ]] && ! pgrep -q ssh-agent; then
    eval "$(ssh-agent -s)"
  fi

  # Add keys from Keychain (silently skip if key doesn't exist or already added)
  for key in ~/.ssh/id_*(N); do
    case "$key" in
      *.pub) continue ;;
    esac
    ssh-add --apple-use-keychain "$key" 2>/dev/null
  done
fi
