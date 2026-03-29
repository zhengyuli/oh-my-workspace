# setup.sh Comprehensive Audit

**Date**: 2026-03-29
**Scope**: `setup.sh` (876 lines, 13 sections)
**Output**: Structured audit report with findings and recommendations

## Dimensions

Each finding is classified along seven dimensions:

| Dimension | What it evaluates |
|-----------|-------------------|
| Correctness | Logic errors, silent failures, race conditions (things that are *wrong*) |
| Style | Formatting, line length (79 chars), indentation, brace placement |
| Comments | WHY-not-WHAT, accuracy, header consistency, noise |
| Rule compliance | Conventions defined in `.claude/rules/bash.md` |
| Feature completeness | Missing error handling, missing features, dead code (things that are *missing*) |
| Security | Injection vectors, eval usage, chmod, temp files, permissions |
| Professionalism | Naming, separation of concerns, single responsibility, DRY |

## Evidence Sources

- **ShellCheck**: Automated static analysis (all SC codes)
- **bash -n**: Syntax validation (passed)
- **bash.md**: Project convention checklist (see Appendix)
- **Manual review**: Line-by-line reading of all 876 lines

### Deduplication

When ShellCheck and manual review produce overlapping findings for the
same line, record them as a single finding with both evidence sources
cited. The findings count reflects unique issues, not evidence sources.

## Section Map

| # | Section | Lines | LOC | Primary focus |
|---|---------|-------|-----|---------------|
| 1 | Bootstrap checks | 21-51 | 31 | Bash version guard, platform guard |
| 2 | Constants | 53-89 | 37 | Naming, readonly, magic numbers |
| 3 | Error handling | 91-107 | 17 | ERR trap, color variables |
| 4 | Logging | 109-152 | 44 | printf usage, confirm() edge cases |
| 5 | Package path helpers | 154-211 | 58 | Nameref, input validation |
| 6 | Stow state queries | 213-287 | 75 | Regex, subshells, find usage |
| 7 | Conflict resolution | 289-324 | 36 | rm -rf safety, DRY_RUN handling |
| 8 | Stow operations | 326-426 | 101 | Stow flags, error paths |
| 9 | Prerequisites | 428-554 | 127 | curl\|bash, eval, Homebrew setup |
| 10 | Commands | 556-769 | 214 | Argument parsing, control flow |
| 11 | Internal helpers | 771-801 | 31 | Shell switch, chsh security |
| 12 | Help | 803-849 | 47 | Help text accuracy |
| 13 | Entry point | 851-876 | 26 | main() structure, dispatch |

## Report Structure

```
1. Executive Summary
   - Overall assessment (qualitative rating)
   - Findings count by severity
   - Top 3 recommendations

2. Audit Methodology
   - Dimensions, evidence sources, section map

3. Section-by-Section Findings
   For each of 13 sections:
   - Section overview (purpose, line range, LOC)
   - Findings: [Severity] [Dimension] [Line(s)] [Description] [Fix]
   - Section qualitative rating

4. Cross-Cutting Concerns
   - Patterns spanning multiple sections

5. Prioritized Recommendations
   - Must-fix (Critical + High severity)
   - Should-fix (Medium severity)
   - Nice-to-have (Low + Info)

6. Appendix
   - Full ShellCheck output
   - bash.md compliance checklist (table below)
```

## Severity Levels

- **Critical**: Data loss, security exposure, broken functionality
- **High**: Features that silently fail or produce wrong results
- **Medium**: Suboptimal patterns that could cause problems
- **Low**: Minor issues unlikely to cause real problems
- **Info**: Observations, no action required

### Severity-to-Recommendation Mapping

| Severity | Recommendation tier |
|----------|---------------------|
| Critical, High | Must-fix |
| Medium | Should-fix |
| Low, Info | Nice-to-have |

## Qualitative Ratings

Each section receives one of:

- **Strong**: No significant issues
- **Good**: Minor issues only, all low/info severity
- **Needs attention**: Medium-severity issues present
- **Needs improvement**: High or critical issues present

### Overall Assessment

The overall script rating equals the worst section rating. One
"Needs improvement" section makes the overall assessment
"Needs improvement," regardless of other sections' ratings.

## Out of Scope

- Other shell scripts in the repository
- Emacs, TOML, YAML, or config files
- Feature requests or new functionality
- Third-party tool behavior (stow, brew, curl)

## Appendix: bash.md Compliance Checklist

| # | Convention | Check |
|---|-----------|-------|
| 1 | Shebang is `#!/usr/bin/env bash` | |
| 2 | File header follows template | |
| 3 | Delimiters at 79 chars, correct hierarchy | |
| 4 | All lines <= 79 chars | |
| 5 | `set -euo pipefail` present | |
| 6 | ERR trap defined | |
| 7 | Variables quoted everywhere | |
| 8 | `[[ ]]` for strings, `(( ))` for arithmetic | |
| 9 | `${VAR:-default}` for defaults | |
| 10 | `printf` used instead of `echo` | |
| 11 | Constants UPPER_SNAKE, locals lower_snake | |
| 12 | No magic numbers | |
| 13 | Max 3 nesting levels, early returns | |
| 14 | Functions have single responsibility | |
| 15 | Parameters validated at function start | |
| 16 | `main()` wraps all logic, called at end | |
| 17 | Separate declaration and assignment for command substitution | |
| 18 | No eval on user input | |
| 19 | No short-circuit operators (`&&`/`||` as flow control) | |
| 20 | Comments explain WHY, not WHAT | |
| 21 | Separate comment lines, no inline explanations | |
