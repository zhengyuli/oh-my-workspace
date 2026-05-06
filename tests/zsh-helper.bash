# zsh_helper.bash — shared utilities for zsh BATS tests

# Root of the zsh config in the dotfiles repo
ZSH_CONF_DIR="${BATS_TEST_DIRNAME}/../shell/zsh"

# Source a zsh module in an isolated environment and evaluate an expression.
# Usage: run_zsh <module_path> [zsh_expression]
#
# IMPORTANT: The expression parameter is interpolated in a bash double-quoted
# string before being passed to zsh. Callers MUST use single-quoted strings
# for the expression (e.g., run_zsh "$MODULE" 'print $FOO'). Double-quoted
# strings with $variables will be expanded by bash, not zsh.
#
# Uses BATS `run` internally so $status and $output are available after call.
run_zsh() {
  local module="$1"
  local expr="${2:-true}"

  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export NO_COLOR=1
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:${BATS_TEST_DIRNAME}/mocks/setup:/usr/bin:/bin\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"

    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"

    source \"${module}\"
    ${expr}
  "
}

setup_zsh_env() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.config/zsh/conf.d"
  mkdir -p "${HOME}/.config/zsh/functions"
  mkdir -p "${HOME}/.config/zsh/completions"
  mkdir -p "${HOME}/.cache/zsh"
  mkdir -p "${HOME}/.local/share"
  mkdir -p "${HOME}/.local/state/zsh"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/mocks/zsh:${BATS_TEST_DIRNAME}/mocks/setup:/usr/bin:/bin"

  export MOCK_BREW_LOG="${BATS_TEST_TMPDIR}/brew.log"
  export MOCK_DEFAULTS_LOG="${BATS_TEST_TMPDIR}/defaults.log"
  export MOCK_OSASCRIPT_LOG="${BATS_TEST_TMPDIR}/osascript.log"
}

teardown_zsh_env() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}
