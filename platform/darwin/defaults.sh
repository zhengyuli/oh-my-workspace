#!/usr/bin/env bash
# defaults.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# macOS System Defaults - developer-optimized
#
# Copyright (C) 2026 zhengyu.li
# Author: zhengyu.li <lizhengyu419@outlook.com>
#
# Usage:       ./defaults.sh
# Dependencies: bash 4.3+, macOS
#
# References:
#   1. mathiasbynens/dotfiles:
#      https://github.com/mathiasbynens/dotfiles/blob/main/.macos
#   2. macOS defaults list: https://macos-defaults.com
#   3. Apple defaults(1) man page: man defaults
# Note: Some changes require logout or restart. Run as your normal user (sudo
#       is used only where required). Close System Settings before running.
#
# Customizations vs. standard mathiasbynens/dotfiles:
#   - Finder new window defaults to $HOME instead of Desktop
#   - Keyboard repeat can go to max (KeyRepeat=1, InitialKeyRepeat=10)
#   - Trackpad corner right-click removed (prone to accidental triggers)
#   - Added: window/QL animation speedup, CrashReporter silence, spring loading
#   - App Store: kept security updates only, disabled auto-install of others
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

# General UI
readonly SIDEBAR_ICON_SIZE_MEDIUM=2
readonly WINDOW_RESIZE_FAST=0.001
readonly STANDBY_DELAY_24H=86400
readonly DISABLE_LINE_MARKS=0

# Keyboard
readonly KEYBOARD_ACCESS_FULL=3
readonly KEY_REPEAT_FASTEST=1
readonly INITIAL_KEY_REPEAT_FAST=10

# Trackpad
readonly TAP_TO_CLICK=1
readonly SPRING_LOAD_NO_DELAY=0

# Screen / screensaver
readonly SCREENSAVER_PASSWORD_REQUIRED=1
readonly SCREENSAVER_PASSWORD_DELAY_IMMEDIATE=0

# Dock
readonly EXPOSE_ANIMATION_FAST=0.1
readonly DOCK_AUTOHIDE_NO_DELAY=0
readonly DOCK_AUTOHIDE_NO_ANIMATION=0

# Terminal
readonly STRING_ENCODING_UTF8=4

# TextEdit
readonly PLAIN_TEXT_MODE=0
readonly TEXT_ENCODING_UTF8=4

# App Store
readonly UPDATE_CHECK_DAILY=1
readonly AUTO_DOWNLOAD_DISABLED=0
readonly CRITICAL_UPDATES_AUTO=1

# Finder
readonly QUICK_LOOK_NO_ANIMATION=0

# -----------------------------------------------------------------------------
# General UI/UX
# -----------------------------------------------------------------------------
_general_ui() {
  printf 'Setting General UI/UX preferences...\n'

  # Set standby delay to 24 hours (AC power only - battery power left at system
  # default to avoid disk/system sleep timing conflicts)
  sudo pmset -c standbydelay "${STANDBY_DELAY_24H}"

  # Disable startup sound
  sudo nvram StartupMute=1

  # Remove duplicates in the "Open With" menu
  /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
      -r -domain local -domain system -domain user

  # Set sidebar icon size to medium (1=small, 2=medium, 3=large)
  defaults write NSGlobalDomain NSTableViewDefaultSizeMode \
      -int "${SIDEBAR_ICON_SIZE_MEDIUM}"

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

  # Speed up window resize animation
  defaults write NSGlobalDomain NSWindowResizeTime \
    -float "${WINDOW_RESIZE_FAST}"

  # Silence crash reporter dialogs
  defaults write com.apple.CrashReporter DialogType -string "none"
}

# -----------------------------------------------------------------------------
# Keyboard
# -----------------------------------------------------------------------------
_keyboard() {
  printf 'Setting Keyboard preferences...\n'

  # Enable full keyboard access for all controls (Tab through all UI elements)
  defaults write NSGlobalDomain AppleKeyboardUIMode \
      -int "${KEYBOARD_ACCESS_FULL}"

  # Maximum keyboard repeat rate (faster than original KeyRepeat=2)
  defaults write NSGlobalDomain KeyRepeat -int "${KEY_REPEAT_FASTEST}"
  defaults write NSGlobalDomain InitialKeyRepeat \
    -int "${INITIAL_KEY_REPEAT_FAST}"

  # Disable press-and-hold in favor of key repeat
  defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
}

# -----------------------------------------------------------------------------
# Trackpad & Mouse
# -----------------------------------------------------------------------------
_trackpad_mouse() {
  printf 'Setting Trackpad/Mouse preferences...\n'

  # Enable tap to click
  defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad \
      Clicking -bool true
  defaults -currentHost write NSGlobalDomain \
      com.apple.mouse.tapBehavior -int "${TAP_TO_CLICK}"
  defaults write NSGlobalDomain \
      com.apple.mouse.tapBehavior -int "${TAP_TO_CLICK}"

  # Right-click via two-finger tap (corner mapping removed - avoids accidents)
  defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad \
      TrackpadRightClick -bool true
  defaults -currentHost write NSGlobalDomain \
      com.apple.trackpad.enableSecondaryClick -bool true

  # Enable spring loading for directories (drag & hover to open)
  defaults write NSGlobalDomain com.apple.springing.enabled -bool true
  defaults write NSGlobalDomain \
      com.apple.springing.delay -float "${SPRING_LOAD_NO_DELAY}"
}

# -----------------------------------------------------------------------------
# Screen
# -----------------------------------------------------------------------------
_screen() {
  printf 'Setting Screen preferences...\n'

  # Require password immediately after sleep or screen saver begins
  defaults write com.apple.screensaver \
      askForPassword -int "${SCREENSAVER_PASSWORD_REQUIRED}"
  defaults write com.apple.screensaver \
      askForPasswordDelay -int "${SCREENSAVER_PASSWORD_DELAY_IMMEDIATE}"

  # Save screenshots to Desktop in PNG format, without drop shadow
  defaults write com.apple.screencapture location -string "${HOME}/Desktop"
  defaults write com.apple.screencapture type -string "png"
  defaults write com.apple.screencapture disable-shadow -bool true
}

# -----------------------------------------------------------------------------
# Finder
# -----------------------------------------------------------------------------
_finder() {
  printf 'Setting Finder preferences...\n'

  # Set $HOME as default for new Finder windows (more useful than Desktop)
  defaults write com.apple.finder NewWindowTarget -string "PfHm"
  defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

  # Show external drives and servers on desktop; hide internal drive
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

  # Search the current folder by default
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

  # Speed up Quick Look panel animation
  defaults write NSGlobalDomain \
      QLPanelAnimationDuration -float "${QUICK_LOOK_NO_ANIMATION}"
}

# -----------------------------------------------------------------------------
# Dock
# -----------------------------------------------------------------------------
_dock() {
  printf 'Setting Dock preferences...\n'

  # Minimize windows into their application's icon
  defaults write com.apple.dock minimize-to-application -bool true

  # Show indicator lights for open applications
  defaults write com.apple.dock show-process-indicators -bool true

  # Speed up Mission Control animations
  defaults write com.apple.dock \
      expose-animation-duration -float "${EXPOSE_ANIMATION_FAST}"

  # Auto-hide Dock instantly (no delay, no animation)
  defaults write com.apple.dock autohide -bool true
  defaults write com.apple.dock autohide-delay \
    -float "${DOCK_AUTOHIDE_NO_DELAY}"
  defaults write com.apple.dock \
      autohide-time-modifier -float "${DOCK_AUTOHIDE_NO_ANIMATION}"

  # Make Dock icons of hidden applications translucent
  defaults write com.apple.dock showhidden -bool true

  # Don't show recent applications in Dock
  defaults write com.apple.dock show-recents -bool false
}

# -----------------------------------------------------------------------------
# Mission Control & Spaces
# -----------------------------------------------------------------------------
_mission_control() {
  printf 'Setting Mission Control & Spaces preferences...\n'

  # Don't auto-rearrange Spaces based on most recent use - keeps Space order
  # stable and prevents fullscreen Spaces from being reshuffled after exit
  defaults write com.apple.dock mru-spaces -bool false

  # Each display has its own Space set (required for per-display fullscreen
  # Spaces to be isolated; prevents cross-display Space leakage)
  defaults write com.apple.spaces spans-displays -bool false

  # Group windows by application in Mission Control - makes it easier to spot
  # and close orphaned Spaces left behind by exited fullscreen windows
  defaults write com.apple.dock expose-group-apps -bool true
}

# -----------------------------------------------------------------------------
# Terminal
# -----------------------------------------------------------------------------
_terminal() {
  printf 'Setting Terminal preferences...\n'

  # Only use UTF-8 (NSStringEncoding 4 = NSUTF8StringEncoding)
  defaults write com.apple.Terminal \
      StringEncodings -array "${STRING_ENCODING_UTF8}"

  # Enable Secure Keyboard Entry; blocks other processes reading keystrokes
  # NOTE: may interfere with some tmux attach setups; disable if needed
  defaults write com.apple.Terminal SecureKeyboardEntry -bool true

  # Disable line marks
  defaults write com.apple.Terminal ShowLineMarks -int "${DISABLE_LINE_MARKS}"
}

# -----------------------------------------------------------------------------
# Time Machine
# -----------------------------------------------------------------------------
_time_machine() {
  printf 'Setting Time Machine preferences...\n'

  # Don't prompt to use new disks as backup volumes
  defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
}

# -----------------------------------------------------------------------------
# TextEdit
# -----------------------------------------------------------------------------
_textedit() {
  printf 'Setting TextEdit preferences...\n'

  # Plain text mode + UTF-8 by default
  defaults write com.apple.TextEdit RichText -int "${PLAIN_TEXT_MODE}"
  defaults write com.apple.TextEdit \
      PlainTextEncoding -int "${TEXT_ENCODING_UTF8}"
  defaults write com.apple.TextEdit \
      PlainTextEncodingForWrite -int "${TEXT_ENCODING_UTF8}"
}

# -----------------------------------------------------------------------------
# Mac App Store
# -----------------------------------------------------------------------------
_app_store() {
  printf 'Setting Mac App Store preferences...\n'

  # Check for updates daily
  defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
  defaults write com.apple.SoftwareUpdate \
      ScheduleFrequency -int "${UPDATE_CHECK_DAILY}"

  # Auto-install security/critical updates only; leave regular updates manual
  defaults write com.apple.SoftwareUpdate \
      AutomaticDownload -int "${AUTO_DOWNLOAD_DISABLED}"
  defaults write com.apple.SoftwareUpdate \
      CriticalUpdateInstall -int "${CRITICAL_UPDATES_AUTO}"
  defaults write com.apple.commerce AutoUpdate -bool false
}

# -----------------------------------------------------------------------------
# Photos
# -----------------------------------------------------------------------------
_photos() {
  printf 'Setting Photos preferences...\n'

  # Prevent Photos from opening automatically when devices are plugged in
  defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true
}

# -----------------------------------------------------------------------------
# Messages
# -----------------------------------------------------------------------------
_messages() {
  printf 'Setting Messages preferences...\n'

  # Disable smart quotes and continuous spell checking
  defaults write com.apple.messageshelper.MessageController \
    SOInputLineSettings -dict-add \
           "automaticQuoteSubstitutionEnabled" -bool false \
           "continuousSpellCheckingEnabled" -bool false
}

# -----------------------------------------------------------------------------
# Kill affected applications
# -----------------------------------------------------------------------------
_restart_apps() {
  printf 'Restarting affected applications...\n'

  local app
  for app in \
      "cfprefsd" \
      "Dock" \
      "Finder" \
      "Messages" \
      "Photos" \
      "SystemUIServer" \
      "Terminal"; do
      if ! killall "${app}" 2>/dev/null; then
          : # Application not running, skip
      fi
  done
}

# =============================================================================
# Main
# =============================================================================

main() {
  # Close System Preferences/Settings to avoid conflicts
  osascript -e 'tell application "System Preferences" to quit' 2>/dev/null
  osascript -e 'tell application "System Settings" to quit' 2>/dev/null

  _general_ui
  _keyboard
  _trackpad_mouse
  _screen
  _finder
  _dock
  _mission_control
  _terminal
  _time_machine
  _textedit
  _app_store
  _photos
  _messages
  _restart_apps

  printf 'Done. Some changes require a logout/restart to take effect.\n'
}

main "$@"
