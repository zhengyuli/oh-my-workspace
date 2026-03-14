# brew.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-12 23:15:00 Thursday by zhengyu.li>
#
# Homebrew helper functions and configuration
#
# Version: 1.1.0

# ==============================================================================
# Completion
# ==============================================================================

# Add Homebrew completion to fpath
if [[ "$(uname)" == "Darwin" ]]; then
    if [[ -d "/opt/homebrew/share/zsh/site-functions" ]]; then
        fpath=("/opt/homebrew/share/zsh/site-functions" $fpath)
    elif [[ -d "/usr/local/share/zsh/site-functions" ]]; then
        fpath=("/usr/local/share/zsh/site-functions" $fpath)
    fi
fi

# ==============================================================================
# Helper Functions
# ==============================================================================

# Show dependencies of a brew package
brdeps() {
    if [[ -z "$1" ]]; then
        echo "Usage: brdeps <package>"
        return 1
    fi
    brew deps --tree "$1"
}

# Show packages that depend on a brew package
brleaves() {
    brew leaves
}

# Show info about a brew package
brinfo() {
    if [[ -z "$1" ]]; then
        echo "Usage: brinfo <package>"
        return 1
    fi
    brew info "$1"
}

# Update brew and upgrade all packages
brupdate() {
    echo "Updating Homebrew..."
    brew update
    echo "Upgrading packages..."
    brew upgrade
    echo "Cleaning up..."
    brew cleanup
    echo "Done!"
}

# List brew packages with descriptions
brlist() {
    brew list | while read formula; do
        echo -n "$formula: "
        brew desc "$formula" 2>/dev/null || echo "no description"
    done
}

# Find brew package
brsearch() {
    if [[ -z "$1" ]]; then
        echo "Usage: brsearch <pattern>"
        return 1
    fi
    brew search "$1"
}
