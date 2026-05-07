# test_helper.bash — shared BATS setup/teardown

# Source setup.sh with an optional workspace directory override.
# Usage:
#   _source_setup                   # Uses real repo as workspace
#   _source_setup "/tmp/workspace"  # Uses custom workspace (for isolation)
_source_setup() {
  local ws="${1:-${BATS_TEST_DIRNAME}/..}"
  export WORKSPACE_DIR="${ws}"
  export BREWFILE="${WORKSPACE_DIR}/pkg-manager/homebrew/Brewfile"
  source "${BATS_TEST_DIRNAME}/../setup.sh"
}

setup() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.config"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/mocks/setup:${PATH}"

  export MOCK_BREW_LOG="${BATS_TEST_TMPDIR}/brew.log"
  export MOCK_BREW_VERSION="4.4.0"
  export MOCK_BREW_BUNDLE_RC=0
  export MOCK_BREW_INSTALL_RC=0
  export MOCK_CURL_LOG="${BATS_TEST_TMPDIR}/curl.log"
  export MOCK_CURL_FAIL_FILE="${BATS_TEST_TMPDIR}/curl_fail_count"
  printf '0' > "${MOCK_CURL_FAIL_FILE}"
  export MOCK_XCODE_RC=0
  export MOCK_UNAME="Darwin"

  unset NO_COLOR
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}
