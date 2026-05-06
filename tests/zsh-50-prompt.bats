#!/usr/bin/env bats
# zsh-50-prompt.bats — tests for conf.d/50-prompt.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/50-prompt.zsh"

@test "starship cache file created" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    [[ -f \"\$XDG_CACHE_HOME/zsh/starship-init.zsh\" ]]
  "
  (( status == 0 ))
}

@test "starship cache reused when fresh" {
  mkdir -p "${HOME}/.cache/zsh"
  echo '# cached' > "${HOME}/.cache/zsh/starship-init.zsh"
  sleep 0.1
  touch "${HOME}/.cache/zsh/starship-init.zsh"

  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    source \"${MODULE}\"
    cat \"\$XDG_CACHE_HOME/zsh/starship-init.zsh\"
  "
  [[ "$output" == *"# cached"* ]]
}

@test "precmd hook registered for terminal title" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export TERM_PROGRAM=\"ghostty\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    print -l \${precmd_functions} | grep -q _omw_set_window_title
  "
  (( status == 0 ))
}

@test "vcs_info fallback when starship unavailable" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    print \$PROMPT
  "
  [[ "$output" == *"%~"* ]]
}
