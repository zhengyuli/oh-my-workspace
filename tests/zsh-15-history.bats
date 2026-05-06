#!/usr/bin/env bats
# zsh-15-history.bats — tests for conf.d/15-history.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/15-history.zsh"

@test "HISTSIZE equals 60000" {
  run_zsh "$MODULE" 'print $HISTSIZE'
  [[ "$output" == "60000" ]]
}

@test "SAVEHIST equals 50000" {
  run_zsh "$MODULE" 'print $SAVEHIST'
  [[ "$output" == "50000" ]]
}

@test "HISTSIZE exceeds SAVEHIST for dup buffer" {
  run_zsh "$MODULE" '(( HISTSIZE > SAVEHIST )) && print yes'
  [[ "$output" == "yes" ]]
}

@test "SHARE_HISTORY enabled" {
  run_zsh "$MODULE" '[[ -o SHARE_HISTORY ]]'
  (( status == 0 ))
}

@test "HIST_IGNORE_SPACE enabled" {
  run_zsh "$MODULE" '[[ -o HIST_IGNORE_SPACE ]]'
  (( status == 0 ))
}

@test "HIST_EXPIRE_DUPS_FIRST enabled" {
  run_zsh "$MODULE" '[[ -o HIST_EXPIRE_DUPS_FIRST ]]'
  (( status == 0 ))
}

@test "HIST_IGNORE_DUPS enabled" {
  run_zsh "$MODULE" '[[ -o HIST_IGNORE_DUPS ]]'
  (( status == 0 ))
}

@test "EXTENDED_HISTORY enabled" {
  run_zsh "$MODULE" '[[ -o EXTENDED_HISTORY ]]'
  (( status == 0 ))
}

@test "HIST_VERIFY enabled" {
  run_zsh "$MODULE" '[[ -o HIST_VERIFY ]]'
  (( status == 0 ))
}

@test "HIST_REDUCE_BLANKS enabled" {
  run_zsh "$MODULE" '[[ -o HIST_REDUCE_BLANKS ]]'
  (( status == 0 ))
}

@test "HIST_SAVE_NO_DUPS enabled" {
  run_zsh "$MODULE" '[[ -o HIST_SAVE_NO_DUPS ]]'
  (( status == 0 ))
}

@test "HIST_FIND_NO_DUPS enabled" {
  run_zsh "$MODULE" '[[ -o HIST_FIND_NO_DUPS ]]'
  (( status == 0 ))
}

@test "HIST_FCNTL_LOCK enabled" {
  run_zsh "$MODULE" '[[ -o HIST_FCNTL_LOCK ]]'
  (( status == 0 ))
}
