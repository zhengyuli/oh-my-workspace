#!/usr/bin/env bats
# zsh-functions.bats — tests for zsh autoloaded functions

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

FUNC_DIR="${BATS_TEST_DIRNAME}/../shell/zsh/zsh/functions"

@test "brew-upgrade: calls update, upgrade, cleanup in order" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
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
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    export MOCK_BREW_LOG=\"${MOCK_BREW_LOG}\"
    export MOCK_BREW_RC=\"${MOCK_BREW_RC}\"
    source \"${FUNC_DIR}/brew-upgrade\"
  "
  [ "$status" -ne 0 ]
}

@test "jsonpp: formats valid JSON from stdin" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin/:/bin\"
    echo '{\"a\":1}' | source \"${FUNC_DIR}/jsonpp\"
  "
  [ "$status" -eq 0 ]
  [[ "$output" == *"\"a\""* ]]
}

@test "jsonpp: formats valid JSON from file argument" {
  local json_file="${BATS_TEST_TMPDIR}/test.json"
  printf '{"b": 2}\n' > "$json_file"
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"/usr/bin:/bin:${BATS_TEST_DIRNAME}/mocks/zsh\"
    source \"${FUNC_DIR}/jsonpp\" \"${json_file}\"
  "
  (( status == 0 ))
  [[ "$output" == *'"b"'* ]]
}

@test "jsonpp: exits non-zero on invalid JSON" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"/usr/bin:/bin:${BATS_TEST_DIRNAME}/mocks/zsh\"
    echo 'not json' | source \"${FUNC_DIR}/jsonpp\" 2>&1
  "
  (( status != 0 ))
  [[ "$output" == *"Expecting"* ]]
}
