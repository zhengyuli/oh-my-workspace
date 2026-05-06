#!/usr/bin/env bats
# zsh-60-keybinds.bats — tests for conf.d/60-keybinds.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/60-keybinds.zsh"

_run_keys() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "emacs keymap selected" {
  _run_keys 'bindkey -lL main'
  [[ "$output" == *"emacs"* ]]
}

@test "KEYTIMEOUT set to 10" {
  _run_keys 'print $KEYTIMEOUT'
  [[ "$output" == "10" ]]
}

@test "sudo-command-line widget registered" {
  _run_keys 'zle -lL | grep sudo-command-line'
  (( status == 0 ))
  [[ "$output" == *"sudo-command-line"* ]]
}

@test "Ctrl+P bound to history-search-backward" {
  _run_keys 'bindkey "^P"'
  [[ "$output" == *"history-search-backward"* ]]
}

@test "Ctrl+N bound to history-search-forward" {
  _run_keys 'bindkey "^N"'
  [[ "$output" == *"history-search-forward"* ]]
}

@test "Ctrl+5C bound to forward-word" {
  _run_keys 'bindkey "^[[1;5C"'
  [[ "$output" == *"forward-word"* ]]
}

@test "Ctrl+5D bound to backward-word" {
  _run_keys 'bindkey "^[[1;5D"'
  [[ "$output" == *"backward-word"* ]]
}

@test "Alt+f bound to forward-word" {
  _run_keys 'bindkey "^[f"'
  [[ "$output" == *"forward-word"* ]]
}

@test "Alt+b bound to backward-word" {
  _run_keys 'bindkey "^[b"'
  [[ "$output" == *"backward-word"* ]]
}

@test "Alt+d bound to kill-word" {
  _run_keys 'bindkey "^[d"'
  [[ "$output" == *"kill-word"* ]]
}

@test "Ctrl+A bound to beginning-of-line" {
  _run_keys 'bindkey "^A"'
  [[ "$output" == *"beginning-of-line"* ]]
}

@test "Ctrl+E bound to end-of-line" {
  _run_keys 'bindkey "^E"'
  [[ "$output" == *"end-of-line"* ]]
}

@test "Ctrl+K bound to kill-line" {
  _run_keys 'bindkey "^K"'
  [[ "$output" == *"kill-line"* ]]
}

@test "Ctrl+U bound to backward-kill-line" {
  _run_keys 'bindkey "^U"'
  [[ "$output" == *"backward-kill-line"* ]]
}

@test "Ctrl+Y bound to yank" {
  _run_keys 'bindkey "^Y"'
  [[ "$output" == *"yank"* ]]
}

@test "Delete key bound to delete-char" {
  _run_keys 'bindkey "^[[3~"'
  [[ "$output" == *"delete-char"* ]]
}

@test "Alt+Backspace bound to backward-kill-word" {
  _run_keys 'bindkey "^[^?"'
  [[ "$output" == *"backward-kill-word"* ]]
}

@test "Ctrl+Del bound to kill-word" {
  _run_keys 'bindkey "^[[3;5~"'
  [[ "$output" == *"kill-word"* ]]
}

@test "Ctrl+L bound to clear-screen" {
  _run_keys 'bindkey "^L"'
  [[ "$output" == *"clear-screen"* ]]
}

@test "Esc Esc bound to sudo-command-line" {
  _run_keys 'bindkey "\e\e"'
  [[ "$output" == *"sudo-command-line"* ]]
}

@test "ghostty Ctrl+H binds backward-kill-word" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export TERM_PROGRAM=\"ghostty\"
    source \"${MODULE}\"
    bindkey '^H'
  "
  [[ "$output" == *"backward-kill-word"* ]]
}
