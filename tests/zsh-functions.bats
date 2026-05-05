#!/usr/bin/env bats
# zsh-functions.bats — tests for zsh autoloaded functions

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

FUNC_DIR="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/functions"

@test "brew-upgrade: calls update, upgrade, cleanup in order" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    export MOCK_BREW_LOG=\"${MOCK_BREW_LOG}\"
    source \"${FUNC_DIR}/brew-upgrade\"
  "
  [ "$status" -eq 0 ]
  # Verify order: update before upgrade before cleanup
  run cat "${MOCK_BREW_LOG}"
  [[ "${lines[0]}" == "update" ]]
  [[ "${lines[1]}" == "upgrade" ]]
  [[ "${lines[2]}" == "cleanup" ]]
}

@test "brew-upgrade: propagates brew failure" {
  export MOCK_BREW_RC=1
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    export MOCK_BREW_LOG=\"${MOCK_BREW_LOG}\"
    export MOCK_BREW_RC=\"${MOCK_BREW_RC}\"
    source \"${FUNC_DIR}/brew-upgrade\"
  "
  [ "$status" -ne 0 ]
}

@test "jsonpp: formats valid JSON from stdin" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin/:/bin\"
    echo '{\"a\":1}' | source \"${FUNC_DIR}/jsonpp\"
  "
  [ "$status" -eq 0 ]
  [[ "$output" == *"\"a\""* ]]
}

@test "jsonpp: passes through invalid JSON error from python3" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    echo 'not json' | source \"${FUNC_DIR}/jsonpp\"
  "
  # python3 writes error to stderr; bat (mock=cat) still exits 0
  # pipefail is reset by emulate -L zsh, so exit code is from bat (0)
  # but stderr should contain the error message
  [[ "$output" == *"Expecting"* ]] || [[ "$status" -ne 0 ]]
}
