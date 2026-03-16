#!/usr/bin/env bash
# ==============================================================================
# defaults.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-16 19:43:32 Monday by zhengyuli>
#
# Copyright (C) 2026 zhengyu li
#
# Author: zhengyuli <lizhengyu419@outlook.com>
# Description: macOS system defaults configuration
#
# Inspired by:
# - https://github.com/mathiasbynens/dotfiles/blob/main/.macos
# - https://github.com/nicknisi/dotfiles/blob/main/install/macos
# ==============================================================================

# ==============================================================================
# Close any open System Preferences panes
# ==============================================================================

osascript -e 'tell application "System Preferences" to quit' 2>/dev/null

# ==============================================================================
# General UI/UX
# ==============================================================================

echo "Setting General UI/UX preferences..."

# Set standby delay to 24 hours (default is 1 hour)
sudo pmset -a standbydelay 86400

# Disable the sound effects on boot
sudo nvram StartupMute=1

# Remove duplicates in the "Open With" menu
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
    -kill -r -domain local -domain system -domain user

# Set sidebar icon size to medium
defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2

# Always show scrollbars
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# ==============================================================================
# Keyboard
# ==============================================================================

echo "Setting Keyboard preferences..."

# Enable full keyboard access for all controls (Tab through all UI elements)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Set a fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# ==============================================================================
# Trackpad & Mouse
# ==============================================================================

echo "Setting Trackpad/Mouse preferences..."

# Trackpad: enable tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Trackpad: map bottom right corner to right-click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true

# ==============================================================================
# Screen
# ==============================================================================

echo "Setting Screen preferences..."

# Require password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Save screenshots to the desktop in PNG format, without drop shadow
defaults write com.apple.screencapture location -string "${HOME}/Desktop"
defaults write com.apple.screencapture type -string "png"
defaults write com.apple.screencapture disable-shadow -bool true

# ==============================================================================
# Finder
# ==============================================================================

echo "Setting Finder preferences..."

# Set Desktop as the default location for new Finder windows
defaults write com.apple.finder NewWindowTarget -string "PfDe"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/Desktop/"

# Show external drives and servers on the desktop; hide internal drive
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

# Show all filename extensions, status bar, and path bar
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
defaults write com.apple.finder ShowStatusBar -bool true
defaults write com.apple.finder ShowPathbar -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Use list view in all Finder windows by default
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Show the ~/Library and /Volumes folders
chflags nohidden ~/Library
sudo chflags nohidden /Volumes

# Expand "General", "Open with", and "Sharing & Permissions" in File Info
defaults write com.apple.finder FXInfoPanesExpanded -dict \
         General -bool true \
         OpenWith -bool true \
         Privileges -bool true

# ==============================================================================
# Dock
# ==============================================================================

echo "Setting Dock preferences..."

# Minimize windows into their application's icon
defaults write com.apple.dock minimize-to-application -bool true

# Show indicator lights for open applications
defaults write com.apple.dock show-process-indicators -bool true

# Speed up Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.1

# Auto-hide Dock instantly (no delay, no animation)
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Don't show recent applications in Dock
defaults write com.apple.dock show-recents -bool false

# ==============================================================================
# Terminal
# ==============================================================================

echo "Setting Terminal preferences..."

# Only use UTF-8 in Terminal.app
defaults write com.apple.terminal StringEncodings -array 4

# Enable Secure Keyboard Entry — prevents other processes from reading
# keystrokes while Terminal is focused
defaults write com.apple.terminal SecureKeyboardEntry -bool true

# Disable line marks
defaults write com.apple.Terminal ShowLineMarks -int 0

# ==============================================================================
# Time Machine
# ==============================================================================

echo "Setting Time Machine preferences..."

# Prevent Time Machine from prompting to use new hard drives as backup volumes
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# ==============================================================================
# TextEdit
# ==============================================================================

echo "Setting TextEdit preferences..."

# Use plain text mode and UTF-8 encoding by default
defaults write com.apple.TextEdit RichText -int 0
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# ==============================================================================
# Mac App Store
# ==============================================================================

echo "Setting Mac App Store preferences..."

# Automatic updates: daily check, background download, install security updates
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
defaults write com.apple.commerce AutoUpdate -bool true

# ==============================================================================
# Photos
# ==============================================================================

echo "Setting Photos preferences..."

# Prevent Photos from opening automatically when devices are plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

# ==============================================================================
# Messages
# ==============================================================================

echo "Setting Messages preferences..."

# Disable smart quotes and continuous spell checking
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add \
         "automaticQuoteSubstitutionEnabled" -bool false \
         "continuousSpellCheckingEnabled" -bool false

# ==============================================================================
# Kill affected applications
# ==============================================================================

echo "Restarting affected applications..."

for app in \
    "cfprefsd" \
        "Dock" \
        "Finder" \
        "Messages" \
        "Photos" \
        "SystemUIServer" \
        "Terminal"; do
    killall "${app}" &>/dev/null
done

echo "Done. Note that some of these changes require a logout/restart to take effect."
