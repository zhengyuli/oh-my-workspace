#!/usr/bin/env bats
# zsh-00-env.bats — tests for conf.d/00-env.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/zsh/conf.d/00-env.zsh"

@test "EDITOR set to nvim" {
  run_zsh "$MODULE" 'print $EDITOR'
  [[ "$output" == "nvim" ]]
}

@test "PAGER set to less" {
  run_zsh "$MODULE" 'print $PAGER'
  [[ "$output" == "less" ]]
}

@test "LANG set to en_US.UTF-8" {
  run_zsh "$MODULE" 'print $LANG'
  [[ "$output" == "en_US.UTF-8" ]]
}

@test "HISTFILE points to XDG_STATE_HOME" {
  run_zsh "$MODULE" 'print $HISTFILE'
  [[ "$output" == *"/.local/state/zsh/history" ]]
}

@test "state and cache dirs created" {
  run_zsh "$MODULE" '
    [[ -d "$XDG_STATE_HOME/zsh" ]] && [[ -d "$XDG_CACHE_HOME/zsh" ]]
  '
  (( status == 0 ))
}

@test "HOMEBREW_NO_AUTO_UPDATE=1" {
  run_zsh "$MODULE" 'print $HOMEBREW_NO_AUTO_UPDATE'
  [[ "$output" == "1" ]]
}

@test "HOMEBREW_NO_ANALYTICS=1" {
  run_zsh "$MODULE" 'print $HOMEBREW_NO_ANALYTICS'
  [[ "$output" == "1" ]]
}

@test "RIPGREP_CONFIG_PATH points to XDG" {
  run_zsh "$MODULE" 'print $RIPGREP_CONFIG_PATH'
  [[ "$output" == *"/.config/ripgrep/rc" ]]
}

@test "STARSHIP_CONFIG points to XDG" {
  run_zsh "$MODULE" 'print $STARSHIP_CONFIG'
  [[ "$output" == *"/.config/starship.toml" ]]
}

@test "FZF_DEFAULT_OPTS contains layout=reverse" {
  run_zsh "$MODULE" 'print $FZF_DEFAULT_OPTS'
  [[ "$output" == *"--layout=reverse"* ]]
}

@test "gdircolors cache created when gdircolors available" {
  run_zsh "$MODULE" '[[ -f "$XDG_CACHE_HOME/zsh/gdircolors.zsh" ]]'
  (( status == 0 ))
}

@test "LS_COLORS fallback when gdircolors unavailable" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export PATH=\"/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"
    source \"${MODULE}\"
    print \$LS_COLORS
  "
  [[ "$output" == *"di=1;34"* ]]
}

@test "GOPATH defaults to XDG_DATA_HOME/go" {
  run_zsh "$MODULE" 'print $GOPATH'
  [[ "$output" == *"/.local/share/go" ]]
}

@test "BUN_INSTALL defaults to XDG_DATA_HOME/bun" {
  run_zsh "$MODULE" 'print $BUN_INSTALL'
  [[ "$output" == *"/.local/share/bun" ]]
}
