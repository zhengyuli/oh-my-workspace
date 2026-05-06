#!/usr/bin/env bats
# zsh-40-plugins.bats — tests for conf.d/40-plugins.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/zsh/conf.d/40-plugins.zsh"

@test "ZINIT_INITIALIZED guard prevents double load" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    typeset -g ZINIT_INITIALIZED=1
    source \"${MODULE}\"
    # If guard works, ZINIT_HOME won't be set
    [[ -z \"\${ZINIT_HOME:-}\" ]]
  "
  (( status == 0 ))
}

@test "ZINIT_HOME points to XDG data dir" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    mkdir -p \"\$XDG_DATA_HOME/zinit/zinit.git\"
    echo '# stub' > \"\$XDG_DATA_HOME/zinit/zinit.git/zinit.zsh\"
    source \"${MODULE}\"
    print \$ZINIT_HOME
  "
  [[ "$output" == *"/.local/share/zinit/zinit.git" ]]
}

@test "returns 1 when zinit.zsh missing" {
  # Create a fake git that always fails so clone doesn't hang
  local fake_bin="${BATS_TEST_TMPDIR}/fake-bin"
  mkdir -p "$fake_bin"
  printf '#!/bin/sh\nexit 1\n' > "$fake_bin/git"
  chmod +x "$fake_bin/git"

  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${fake_bin}:/usr/bin:/bin\"
    export NO_COLOR=1
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
  "
  (( status == 1 ))
}
