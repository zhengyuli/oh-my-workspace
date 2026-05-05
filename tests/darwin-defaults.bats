#!/usr/bin/env bats
# darwin-defaults.bats — tests for platform/darwin/defaults.sh

setup() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/zsh-bin:${BATS_TEST_DIRNAME}/setup-bin:/usr/bin:/bin"

  export MOCK_DEFAULTS_LOG="${BATS_TEST_TMPDIR}/defaults.log"
  export MOCK_OSASCRIPT_LOG="${BATS_TEST_TMPDIR}/osascript.log"
  touch "${MOCK_DEFAULTS_LOG}" "${MOCK_OSASCRIPT_LOG}"

  source "${BATS_TEST_DIRNAME}/../platform/darwin/defaults.sh"
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}

@test "sourcing does not run main" {
  [ ! -s "${MOCK_DEFAULTS_LOG}" ]
}

@test "_general_ui calls defaults write NSGlobalDomain" {
  run _general_ui
  [ "$status" -eq 0 ]
  grep -q "NSGlobalDomain" "${MOCK_DEFAULTS_LOG}"
}

@test "_keyboard sets KeyRepeat" {
  run _keyboard
  [ "$status" -eq 0 ]
  grep -q "KeyRepeat" "${MOCK_DEFAULTS_LOG}"
}

@test "_trackpad_mouse enables tap to click" {
  run _trackpad_mouse
  [ "$status" -eq 0 ]
  grep -q "Clicking" "${MOCK_DEFAULTS_LOG}"
}

@test "_finder shows all extensions" {
  run _finder
  [ "$status" -eq 0 ]
  grep -q "AppleShowAllExtensions" "${MOCK_DEFAULTS_LOG}"
}

@test "_dock enables autohide" {
  run _dock
  [ "$status" -eq 0 ]
  grep -q "autohide" "${MOCK_DEFAULTS_LOG}"
}

@test "_time_machine disables new disk prompt" {
  run _time_machine
  [ "$status" -eq 0 ]
  grep -q "DoNotOfferNewDisksForBackup" "${MOCK_DEFAULTS_LOG}"
}

@test "_restart_apps calls osascript" {
  # main() calls osascript to quit System Preferences/Settings before config
  run main
  [ "$status" -eq 0 ]
  grep -qi "quit" "${MOCK_OSASCRIPT_LOG}"
}
