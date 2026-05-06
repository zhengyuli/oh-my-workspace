#!/usr/bin/env bats
# zsh-10-options.bats — tests for conf.d/10-options.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/10-options.zsh"

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

@test "PUSHD_IGNORE_DUPS enabled" {
  run_zsh "$MODULE" '[[ -o PUSHD_IGNORE_DUPS ]]'
  (( status == 0 ))
}

@test "PUSHD_SILENT enabled" {
  run_zsh "$MODULE" '[[ -o PUSHD_SILENT ]]'
  (( status == 0 ))
}

@test "CDABLE_VARS enabled" {
  run_zsh "$MODULE" '[[ -o CDABLE_VARS ]]'
  (( status == 0 ))
}

@test "ALWAYS_TO_END enabled" {
  run_zsh "$MODULE" '[[ -o ALWAYS_TO_END ]]'
  (( status == 0 ))
}

@test "AUTO_LIST enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_LIST ]]'
  (( status == 0 ))
}

@test "AUTO_PARAM_SLASH enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_PARAM_SLASH ]]'
  (( status == 0 ))
}

@test "COMPLETE_IN_WORD enabled" {
  run_zsh "$MODULE" '[[ -o COMPLETE_IN_WORD ]]'
  (( status == 0 ))
}

@test "AUTO_MENU enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_MENU ]]'
  (( status == 0 ))
}

@test "GLOB_DOTS enabled" {
  run_zsh "$MODULE" '[[ -o GLOB_DOTS ]]'
  (( status == 0 ))
}

@test "NUMERIC_GLOB_SORT enabled" {
  run_zsh "$MODULE" '[[ -o NUMERIC_GLOB_SORT ]]'
  (( status == 0 ))
}

@test "RC_QUOTES enabled" {
  run_zsh "$MODULE" '[[ -o RC_QUOTES ]]'
  (( status == 0 ))
}

@test "COMBINING_CHARS enabled" {
  run_zsh "$MODULE" '[[ -o COMBINING_CHARS ]]'
  (( status == 0 ))
}

@test "RM_STAR_SILENT disabled" {
  run_zsh "$MODULE" '[[ ! -o RM_STAR_SILENT ]]'
  (( status == 0 ))
}

@test "AUTO_RESUME enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_RESUME ]]'
  (( status == 0 ))
}

@test "LONG_LIST_JOBS enabled" {
  run_zsh "$MODULE" '[[ -o LONG_LIST_JOBS ]]'
  (( status == 0 ))
}

@test "NOTIFY enabled" {
  run_zsh "$MODULE" '[[ -o NOTIFY ]]'
  (( status == 0 ))
}

@test "BG_NICE disabled" {
  run_zsh "$MODULE" '[[ ! -o BG_NICE ]]'
  (( status == 0 ))
}

@test "CHECK_JOBS disabled" {
  run_zsh "$MODULE" '[[ ! -o CHECK_JOBS ]]'
  (( status == 0 ))
}

@test "HUP disabled" {
  run_zsh "$MODULE" '[[ ! -o HUP ]]'
  (( status == 0 ))
}
