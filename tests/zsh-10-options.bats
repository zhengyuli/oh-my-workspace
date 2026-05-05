#!/usr/bin/env bats
# zsh-10-options.bats — tests for conf.d/10-options.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/10-options.zsh"

@test "AUTO_CD enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_CD ]]'
  (( status == 0 ))
}

@test "EXTENDED_GLOB enabled" {
  run_zsh "$MODULE" '[[ -o EXTENDED_GLOB ]]'
  (( status == 0 ))
}

@test "BEEP disabled" {
  run_zsh "$MODULE" '[[ ! -o BEEP ]]'
  (( status == 0 ))
}

@test "INTERACTIVE_COMMENTS enabled" {
  run_zsh "$MODULE" '[[ -o INTERACTIVE_COMMENTS ]]'
  (( status == 0 ))
}

@test "AUTO_PUSHD enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_PUSHD ]]'
  (( status == 0 ))
}

@test "FLOW_CONTROL disabled" {
  run_zsh "$MODULE" '[[ ! -o FLOW_CONTROL ]]'
  (( status == 0 ))
}

@test "NULL_GLOB enabled" {
  run_zsh "$MODULE" '[[ -o NULL_GLOB ]]'
  (( status == 0 ))
}

@test "CLOBBER disabled" {
  run_zsh "$MODULE" '[[ ! -o CLOBBER ]]'
  (( status == 0 ))
}
