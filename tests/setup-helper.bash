# test_helper.bash — shared BATS setup/teardown

_source_setup() {
  export WORKSPACE_DIR="${BATS_TEST_DIRNAME}/.."
  export BREWFILE="${WORKSPACE_DIR}/pkg-manager/homebrew/Brewfile"
  source "${WORKSPACE_DIR}/setup.sh"
}

setup() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.config"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/mocks/setup:${PATH}"

  export MOCK_STOW_LOG="${BATS_TEST_TMPDIR}/stow.log"
  export MOCK_STOW_OUTPUT=""
  export MOCK_STOW_STDERR=""
  export MOCK_STOW_RC=0
  export MOCK_BREW_LOG="${BATS_TEST_TMPDIR}/brew.log"
  export MOCK_BREW_VERSION="4.4.0"
  export MOCK_BREW_BUNDLE_RC=0
  export MOCK_BREW_INSTALL_RC=0
  export MOCK_CURL_LOG="${BATS_TEST_TMPDIR}/curl.log"
  export MOCK_CURL_FAIL_FILE="${BATS_TEST_TMPDIR}/curl_fail_count"
  echo "0" > "${MOCK_CURL_FAIL_FILE}"
  export MOCK_XCODE_RC=0
  export MOCK_UNAME="Darwin"

  unset NO_COLOR
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}
