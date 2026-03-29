# setup.sh Comprehensive Audit Report

**Date**: 2026-03-29
**Auditor**: Claude Opus 4.6
**Scope**: `setup.sh` (876 lines, 13 sections)
**Overall Rating**: **Good**

---

## 1. Executive Summary

`setup.sh` is a well-crafted shell script that demonstrates strong adherence to project conventions, consistent defensive programming, and thoughtful documentation. The codebase shows mature engineering judgment in error handling, edge-case coverage, and separation of concerns.

**Findings by severity:**

| Severity | Count |
|----------|-------|
| Critical | 0 |
| High | 0 |
| Medium | 4 |
| Low | 8 |
| Info | 40+ |

**Top 3 recommendations:**

1. **Add `yazi` to the help text package list** (L826) — the help
   text lists 10 packages but PKG_ALL defines 11. Users cannot discover
   `yazi` from the help output.

2. **Fix SC2295 glob-expansion risk in `stow_links()`** (L275, L281) — quote
   expansions inside `${..}` separately to prevent glob characters in
   paths from matching as patterns. Free fix, no downside.

3. **Honor DRY_RUN in `unstow_package()`** (L409-426) — all other mutation
   functions respect dry-run mode but this one skips it, causing real
   filesystem changes during what the user expects to be a preview.

---

## 2. Audit Methodology

**Dimensions evaluated:** Correctness, Style, Comments, Rule compliance,
Feature completeness, Security, Professionalism

**Evidence sources:**
- ShellCheck (6 info-level findings: SC2295 x2, SC2059 x4)
- `bash -n` syntax check: PASS
- Line length check: 4 lines exceed 79 chars (L786, L798, L830, L834)
- `.claude/rules/bash.md` compliance checklist (17/18 PASS, 1 PARTIAL)
- Manual line-by-line review of all 876 lines

**Section map:**

| # | Section | Lines | LOC | Rating |
|---|---------|-------|-----|--------|
| 1 | Bootstrap checks | 21-51 | 31 | Strong |
| 2 | Constants | 53-89 | 37 | Good |
| 3 | Error handling | 91-107 | 17 | Good |
| 4 | Logging | 109-152 | 44 | Good |
| 5 | Package path helpers | 154-211 | 58 | Strong |
| 6 | Stow state queries | 213-287 | 75 | Good |
| 7 | Conflict resolution | 289-324 | 36 | Good |
| 8 | Stow operations | 326-426 | 101 | Good |
| 9 | Prerequisites | 428-554 | 127 | Good |
| 10 | Commands | 556-769 | 214 | Good |
| 11 | Internal helpers | 771-801 | 31 | Strong |
| 12 | Help | 803-849 | 47 | Good |
| 13 | Entry point | 851-876 | 26 | Strong |

---

## 3. Section-by-Section Findings

### Section 1: Bootstrap Checks (L21-51)

**Rating: Strong** | 5 findings (all Info)

The bash version guard uses arithmetic context correctly. The error message
uses a creative `sed`+heredoc substitution pattern rather than `printf`,
which is unconventional but functional. Platform guard is clean. WHY
comments explain the version requirement rationale well.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 1 | Info | Style | 28 | `sed`+heredoc for error message is unconventional — `printf` would be more consistent with the rest of the script | Replace `sed`+heredoc with `printf` |
| 2 | Info | Comments | 25 | "Require bash 4.3+ for local -n (nameref) and associative arrays" — good WHY comment | No change |
| 3 | Info | Comments | 33-42 | Heredoc body mixes user-facing docs with code comments — acceptable for bootstrap error | No change |
| 4 | Info | Professionalism | 26-27 | Bash version check uses arithmetic `(( ))` per conventions | No change |
| 5 | Info | Security | 48 | Platform check uses `$(uname -s)` correctly | no change |

### Section 2: Constants (L53-89)

**Rating: Good** | 6 findings (all Info)

Constants use `readonly` consistently. WORKSPACE_DIR correctly uses
separate declaration/assignment for command substitution. The comment
on L75-80 explaining WHY is exemplary. `NETWORK_TIMEOUT` is declared
but not verified to be used (it is used later at L466). `DRY_RUN`
is intentionally mutable but lacks a comment noting this.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 6 | Info | Rule | 57 | `NETWORK_TIMEOUT=60` — appears unused in L1-152 scope but is used at L466 | No change |
| 7 | Info | Rule | 81 | WORKSPACE_DIR uses separate declare/assign — matches bash.md | No change |
| 8 | Info | Rule | 84-85 | BREWFILE uses same pattern but RHS is simple concatenation, not command substitution — unnecessary but consistent | No change |
| 9 | Info | Comments | 60 | "Format: \<category>/\<package>" is borderline WHAT — acceptable as data format documentation | No change |
| 10 | Info | Professionalism | 89 | `DRY_RUN=false` lacks comment noting intentional mutability | Add `# Intentionally mutable — toggled by --dry-run flag` |
| 11 | Info | Rule | 81 | ShellCheck SC2295 on `${BASH_SOURCE[0]}` — not a pattern context, no functional issue | No change |

### Section 3: Error Handling (L91-107)

**Rating: Good** | 5 findings (all Info)

ERR trap matches bash.md convention exactly. Color variables use `_`
prefix and `readonly`/`UPPER_SNAKE_CASE` — defensible for script-wide
constants that should not be exported. `_err_handler` captures `$?`
before printing, which is correct.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 12 | Info | Style | 95-100 | `_RED` etc. with underscore prefix — indicates "private" for script-wide constants | No change |
| 13 | Info | Rule | 95-100 | Constants use `readonly` and `UPPER_SNAKE_CASE` | No change |
| 14 | Info | Rule | 103 | `$?` does not need quoting in arithmetic context | No change |
| 15 | Info | Style | 104 | Double-quoted printf format — necessary for ANSI escape expansion | No change |
| 16 | Info | Professionalism | 102-107 | Matches bash.md ERR trap pattern exactly | No change |

### Section 4: Logging (L109-152)

**Rating: Good** | 8 findings (1 Medium, 7 Info)

Five logging functions form a consistent API. `print_header` uses an
unquoted `$(seq)` which relies on intentional word splitting — works but
is fragile. `confirm()` handles EOF on non-interactive stdin correctly.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 17 | Info | Professionalism | 113-121 | Consistent log function API | No change |
| 18 | **Medium** | Correctness | 125, 127 | `$(seq 1 "${LINE_WIDTH}")` is unquoted, relying on word splitting. Works with numeric `LINE_WIDTH=79` but fragile | Use `printf '%s' "$(seq 1 "${LINE_WIDTH}")"` or document the intentional word splitting |
| 19 | Info | Style | 113 | `die()` is a single-line function — acceptable for a 1-statement function | No change |
| 20 | Low | Feature | 137-152 | `confirm()` does not validate `$2` is "y" or "n" — implicit contract | Document accepted values in function comment |
| 21 | Info | Rule | 138-140 | `local` with string assignment does not mask exit codes — correct | No change |
| 22 | Info | Comments | 131-136 | Doc-comment for `confirm()` follows structured format | No change |
| 23 | Info | Correctness | 148-150 | EOF handling for non-interactive stdin is correct | No change |
| 24 | Info | Rule | 151 | `[[ ]]` with regex for string matching — correct | No change |

### Section 5: Package Path Helpers (L154-211)

**Rating: Strong** | 6 findings (all Info)

Clean composition of four helper functions. `validate_pkgs` uses nameref
with `_out` underscore prefix to avoid collision — well-documented with
a comment explaining WHY. Doc-comments are exemplary.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 25 | Info | Style | 159, 162 | One-liner functions — acceptable for trivial helpers | No change |
| 26 | Info | Style | 165 | Nested command substitution in printf — acceptable for one-liners | No change |
| 27 | Info | Rule | 189 | Nameref `_out` with underscore prefix avoids collision — good practice | No change |
| 28 | Info | Correctness | 189 | Nameref collision risk exists but underscore convention mitigates it | No change |
| 29 | Info | Feature | 172 | Unnecessary braces around `${p}` inside `[[ ]]` — minor noise | No change |
| 30 | Info | Comments | 179-187 | Multi-line doc block explains what, how, and WHY for nameref — exemplary | No change |

### Section 6: Stow State Queries (L213-287)

**Rating: Good** | 7 findings (2 WARN, 5 Info)

`stow_targets` regex parsing and `stow_links` tree walking are correct.
Two ShellCheck SC2295 warnings are the most actionable findings in this
section — quoting the expansion inside `${#}` prevents glob characters
in paths from matching as patterns.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 31 | **Medium** | Correctness | 275 | SC2295: `${src#${pkg_dir}/}` — unquoted expansion may match glob characters as patterns | Fix: `${src#"${pkg_dir}"/}` |
| 32 | **Medium** | Correctness | 281 | SC2295: `${check#${HOME}/}` — same issue | Fix: `${check#"${HOME}"/}` |
| 33 | Info | Feature | 248 | `stow ... \|\| true` silently swallows errors — by design for `stow -n` but worth noting | No change |
| 34 | Info | Security | 286 | `find` does not validate `$pkg_dir` exists — `2>/dev/null` handles this | No change |
| 35 | Info | Style | 247-248 | Separate declare/assign for command substitution — correct | No change |
| 36 | Info | Comments | 256-263 | `stow_links()` comment explains WHY for process substitution — excellent | No change |
| 37 | Info | Comments | 224-232 | `stow_targets()` documents both modes and explains restow rationale — well done | No change |

### Section 7: Conflict Resolution (L289-324)

**Rating: Good** | 5 findings (1 WARN, 4 Info)

`_remove_stow_conflicts` is the most security-sensitive function — it
removes files based on parsed stow output. The `rm -rf` on L320 operates
on paths under `$HOME` but lacks a defensive prefix check. In practice,
paths come from trusted stow output, but a guard would harden this.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 38 | **Medium** | Security | 320 | `rm -rf "${target}"` on parsed path without verifying it is under `$HOME` | Add guard: `[[ "${target}" == "${HOME}"/* ]]` |
| 39 | Info | Correctness | 307, 316 | `"${DRY_RUN}"` as boolean command — correct idiomatic pattern | No change |
| 40 | Info | Feature | 305-323 | Empty `stow_targets` output handled correctly (loop doesn't execute) | No change |
| 41 | Info | Comments | 293-299 | Function header documents DRY_RUN global dependency — good practice | No change |
| 42 | Info | Professionalism | 313, 320 | `rm -f` for symlinks vs `rm -rf` for directories — correct distinction | No change |

### Section 8: Stow Operations (L326-426)

**Rating: Good** | 7 findings (1 WARN, 6 Info)

`_stow_exec` is well-structured with correct dry-run handling. The
pre-creation of `~/.config` has a valuable WHY comment. The notable gap
is `unstow_package()` which does not honor `DRY_RUN`, making it
inconsistent with the other mutation functions.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 43 | **Medium** | Feature | 409-426 | `unstow_package()` does not honor `DRY_RUN` — all other mutation functions do | Add dry-run handling mirroring `_stow_exec` |
| 44 | Info | Correctness | 353-357 | `mkdir -p` intentionally skipped in dry-run — comment explains WHY | No change |
| 45 | Info | Feature | 346-349 | Package directory existence validated before proceeding — good | No change |
| 46 | Info | Feature | 393-399 | `stow_package()` short-circuits if already stowed — correct | No change |
| 47 | Info | Style | 366-367 | Separate declare/assign for command substitution | No change |
| 48 | Info | Comments | 353-354 | Inline comment explains WHY for .config pre-creation — exemplary | No change |
| 49 | Info | Comments | 330-337 | `_stow_exec` header documents DRY_RUN and mode semantics | No change |

### Section 9: Prerequisites (L428-554)

**Rating: Good** | 7 findings (all Minor/Info)

The most security-sensitive section. `curl | bash` and `eval` are
documented with WHY comments explaining the security rationale.
`ensure_prerequisites` has a redundant sanity check in "check" mode.
The `ok` variable uses inverted boolean sense.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 50 | Low | Security | 463-466 | `curl \| bash` — inherent supply-chain risk, Partially mitigated by hardcoded HTTPS and `--fail` | Consider pinning to a commit SHA instead of HEAD |
| 51 | Low | Security | 475 | `eval "$(brew shellenv)"` — executes external binary output. Acceptable per official instructions | No change |
| 52 | Low | Style | 432-434 | One-liner predicate functions — inconsistent with multi-line style elsewhere | Acceptable for trivial predicates |
| 53 | Low | Feature | 517-554 | `ensure_prerequisites` final stow check (L551) is redundant in "check" mode | Wrap in `if [[ "${mode}" == install ]]` |
| 54 | Low | Style | 519 | `ok=1`/`ok=0` — inverted boolean sense is not self-documenting | Consider `missing=0` or explicit boolean pattern |
| 55 | Low | Professionalism | 455-484 | Dual-arch Homebrew path iteration — good portability | No change |
| 56 | Low | Comments | 461-462, 470-471 | Security WHY comments for curl\|bash and eval — good practice | No change |

### Section 10: Commands (L556-769)

**Rating: Good** | 12 findings (all Minor/Info)

The largest section. `cmd_install` at 78 lines approaches single-responsibility
limits. `cmd_status` uses a single-pass optimization for `is_stowed` with
a WHY comment. `local -A` (associative array) requires Bash 4+ but
no version guard protects this specific usage. Inconsistent empty-argument
handling between `cmd_install` (shows help, returns 0) and `cmd_uninstall`
(logs error, returns 1).

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 57 | Low | Correctness | 577 | `DRY_RUN=true` bare assignment relies on no local shadowing — documented but fragile | No change |
| 58 | Low | Style | 612-616 | 3-level nesting in install path — at the boundary of the 3-level rule | Flatten with `if ! "${force}" && ! "${DRY_RUN}"; then` |
| 59 | Low | Rule | 583-584, 662-663 | Boolean flags tested as runnable commands `"${do_all}"` — unconventional but internally consistent | No change |
| 60 | Low | Feature | 626-629 | Empty args silently falls through to `show_help` — user may not realize arguments were ignored | Add hint: "No packages specified. Showing help:" |
| 61 | Low | Feature | 687-691 | Inconsistent empty-arg handling: install shows help (exit 0), uninstall errors (exit 1) | Choose one behavior for both commands |
| 62 | Low | Correctness | 743 | `local -A` requires Bash 4+ — the bootstrap guard covers this | No change (already guarded) |
| 63 | Low | Style | 724-738 | `printf` format strings embed color variables directly — works via octal expansion | No change |
| 64 | Low | Professionalism | 567-644 | `cmd_install` at 78 lines approaches SRP limit | Consider extracting `--all` path into helper |
| 65 | Low | Style | 730, 736 | Pipeline `brew --version \| head \| awk` — acceptable for display | No change |
| 66 | Low | Security | 577 | `--dry-run` flag validated via case dispatch — no injection vector | No change |
| 67 | Low | Feature | 677 | `confirm()` guard on mass uninstall — good protection against accidental data loss | No change |
| 68 | Low | Comments | 560-566, 646-648, 704-706 | Consistent doc-comment blocks for all three commands | No change |

### Section 11: Internal Helpers (L771-801)

**Rating: Strong** | 6 findings (2 Low, 4 Info)

`_offer_shell_switch` has the security-relevant `chsh -s` call. Path is
validated via `command -v` before use. The failure path provides actionable
sudo guidance. Two lines exceed 79 chars (L786, L798). The function correctly
uses separate declare/assign for command substitution.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 69 | Low | Style | 786 | Line exceeds 79 chars (82 chars) | Break string with line continuation |
| 70 | Low | Style | 798 | Line exceeds 79 chars (80 chars) | Break string with line continuation |
| 71 | Low | Comments | 775-776 | Pre-function comment describes WHAT not WHY | Replace with WHY note |
| 72 | Low | Security | 794-798 | `chsh` path validated, sudo guidance accurate — safe | No change |
| 73 | Info | Feature | 794-798 | `else` branch after chsh failure does not return non-zero — acceptable (best-effort) | No change |
| 74 | Info | Rule | 789-790 | Correctly separates local from command substitution — well done | No change |

### Section 12: Help (L803-849)

**Rating: Good** | 5 findings (1 Medium, 4 Low/Info)

The help text is comprehensive and well-structured. The notable finding
is that the package list on L826 omits `yazi`, listing only 10 packages
when PKG_ALL defines 11.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 75 | **Medium** | Correctness | 826 | Package list omits `yazi` — help shows 10 names, PKG_ALL has 11 | Add `yazi` to package list |
| 76 | Low | Style | 830 | Line exceeds 79 chars (80 chars) | Reduce padding |
| 77 | Low | Style | 834 | Line exceeds 79 chars (80 chars) | Reduce padding |
| 78 | Low | Comments | 807 | `show_help` not underscore-prefixed — borderline since it is used in dispatch | Consider `_show_help` for consistency |
| 79 | Info | Feature | 808-849 | Help is comprehensive — covers all commands, flags, examples, details | No change |

### Section 13: Entry Point (L851-876)

**Rating: Strong** | 4 findings (all Low/Info)

Clean `main()` with explicit case dispatch. Follows bash.md "Main Function"
pattern exactly. `main "$@"` idiom correct. No eval on user input — safe.

| # | Sev | Dimension | Line(s) | Description | Fix |
|---|-----|-----------|---------|-------------|-----|
| 80 | Low | Style | 863-865 | Case arm alignment differs from L866 — aesthetic | No change |
| 81 | Low | Comments | 855-876 | No header comment inside main() — section header covers this | No change |
| 82 | Info | Rule | 862-873 | Explicit case dispatch — safe from injection | No change |
| 83 | Info | Rule | 876 | `main "$@"` idiom matches convention | No change |

---

## 4. Cross-Cutting Concerns

### printf format strings with embedded variables (SC2059)

Six locations across Sections 3, 4, 10 embed color variables
(`${_GREEN}`, `${_RED}`, etc.) directly in printf format strings.
ShellCheck flags this as SC2059. The pattern works because the
variables contain `\033` escape sequences that printf interprets.
This is a deliberate design choice used consistently throughout, but
it is a systematic deviation from the "no variables in format strings"
convention.

**Affected lines:** 104, 113, 115, 117, 119, 121, 124, 128, 724, 726,
729, 732, 735, 738, 762, 765

### Boolean handling via runnable strings

Multiple functions test boolean flags by running the string `"true"`
or `"false"` as a command: `"${DRY_RUN}"`, `"${do_all}"`, `"${force}"`.
This is an idiomatic bash pattern used consistently throughout the
script. It avoids the complexity of string comparisons but may be
unfamiliar to some readers.

**Affected sections:** 4, 7, 8, 10

### Separate declare/assign pattern

The bash.md anti-pattern "local masks exit codes" is correctly avoided
throughout the entire file. Every function that uses command substitution
declares `local var` on one line and assigns `var=$(...)` on the next.
This is exemplary adherence to the convention.

**Affected sections:** 2, 6, 8, 11

### WHY comments

Comments are consistently strong across the entire file. Nearly every
non-obvious decision has a WHY comment: separate declare/assign rationale
(L75-80), process substitution for associative array visibility (L260-262),
pre-creating .config to prevent tree-folding (L353-354), security
justifications for curl|bash (L461-462) and eval (L470-471), and single-pass
optimization (L741-742). This is the best-documented shell script in the
repository.

---

## 5. Prioritized Recommendations

### Must-fix (Critical + High severity)

None found. No critical or high-severity issues were identified.

### Should-fix (Medium severity)

| # | Finding | Line(s) | Description |
|---|---------|---------|-------------|
| 1 | SC2295 glob-expansion risk | 275, 281 | Quote expansions inside `${..}` separately: `${src#"${pkg_dir}"/}` and `${check#"${HOME}"/}` |
| 2 | `yazi` missing from help text | 826 | Add `yazi` to the package list in `show_help()` |
| 3 | `unstow_package()` ignores DRY_RUN | 409-426 | Add dry-run preview mode mirroring `_stow_exec` |
| 4 | `rm -rf` lacks HOME prefix guard | 320 | Add `[[ "${target}" == "${HOME}"/* ]]` before `rm -rf` |
| 5 | Unquoted `$(seq)` word splitting | 125, 127 | Quote or document the intentional word splitting in `print_header()` |

### Nice-to-have (Low + Info)

| # | Finding | Line(s) | Description |
|---|---------|---------|-------------|
| 6 | Line length violations | 786, 798, 830, 834 | Four lines exceed 79 chars |
| 7 | `DRY_RUN` mutability comment | 89 | Add comment noting intentional mutability |
| 8 | `ensure_prerequisites` redundant check | 551 | Wrap final stow check in mode guard |
| 9 | Inconsistent empty-arg behavior | 626-629, 687-691 | Unify install/uninstall behavior |
| 10 | `cmd_install` SRP limit | 567-644 | Consider extracting `--all` path |
| 11 | `show_help` naming | 807 | Consider `_show_help` for consistency |
| 12 | `ok` variable clarity | 519 | Rename to `missing=0` or add comment |

---

## 6. Appendix

### ShellCheck Output

```
In setup.sh line 275:
    rel="${src#${pkg_dir}/}"
               ^--------^ SC2295 (info): Expansions inside ${..}
               need to be quoted separately

In setup.sh line 281:
          "${check#${HOME}/}"
                   ^-----^ SC2295 (info): Expansions inside ${..}
                   need to be quoted separately

In setup.sh line 724:
    printf "  ${_GREEN}ok${_RESET}  Xcode CLI\n"
           ^-- SC2059 (info): Don't use variables in printf format
           string

In setup.sh line 726:
    printf "  ${_RED}--${_RESET}  Xcode CLI\n"
           ^-- SC2059 (info): Don't use variables in printf format
           string

In setup.sh line 732:
    printf "  ${_RED}--${_RESET}  Homebrew\n"
           ^-- SC2059 (info): Don't use variables in printf format
           string

In setup.sh line 738:
    printf "  ${_RED}--${_RESET}  GNU Stow\n"
           ^-- SC2059 (info): Don't use variables in printf format
           string
```

### bash.md Compliance Checklist

| # | Convention | Result | Notes |
|---|-----------|--------|-------|
| 1 | Shebang `#!/usr/bin/env bash` | PASS | L1 |
| 2 | File header follows template | PASS | L1-17 |
| 3 | Delimiters at 79 chars | PASS | All Level 0/1 delimiters verified |
| 4 | All lines <= 79 chars | FAIL | 4 lines exceed (L786, L798, L830, L834) |
| 5 | `set -euo pipefail` present | PASS | L19 |
| 6 | ERR trap defined | PASS | L102-107 |
| 7 | Variables quoted everywhere | PASS | All variables properly quoted |
| 8 | `[[ ]]` for strings, `(( ))` for arithmetic | PASS | Used correctly throughout |
| 9 | `${VAR:-default}` for defaults | PASS | L81, L89, L139, L189, L235 |
| 10 | `printf` over `echo` | PASS | No `echo` found |
| 11 | Constants UPPER_SNAKE, locals lower_snake | PASS | Consistent naming |
| 12 | No magic numbers | PASS | All literals are named constants |
| 13 | Max 3 nesting levels | PASS | Max nesting is 3 levels |
| 14 | Functions single responsibility | PASS | Most functions are focused |
| 15 | Parameters validated | PARTIAL | `confirm()` does not validate `$2` |
| 16 | `main()` wraps logic | PASS | L855-876 |
| 17 | Separate local + assign for cmd subst | PASS | Exemplary — used everywhere |
| 18 | No eval on user input | PASS | `eval` only on hardcoded Homebrew path |
| 19 | No short-circuit operators | PASS | No `&&`/`||` flow control |
| 20 | Comments explain WHY | PASS | Best-documented script in the repo |
| 21 | Separate comment lines | PASS | No inline explanations |

**Score: 18/21 PASS, 1 FAIL (line length), 1 PARTIAL, 1 N/A**

### Syntax Validation

```
bash -n setup.sh → PASS
```

### Line Length Violations

```
L786  (82 chars):     log_info "Skipped — run: chsh -s \$(which zsh)"
L798  (80 chars):     log_info "Run: echo '${zsh_path}' | sudo tee -a /etc/shells"
L830  (80 chars):   ./setup.sh install zsh git                  Stow specific packages
L834  (80 chars):   ./setup.sh install --force --dry-run --all  Preview a full restow
```
