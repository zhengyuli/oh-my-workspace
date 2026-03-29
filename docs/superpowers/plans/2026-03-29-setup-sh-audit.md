# setup.sh Comprehensive Audit — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a structured audit report for `setup.sh` evaluating all 13 sections across 7 dimensions (correctness, style, comments, rule compliance, feature completeness, security, professionalism).

**Architecture:** Collect automated evidence first (ShellCheck, line length, convention checklist), then dispatch parallel subagents to audit section groups, then synthesize into a single report at `docs/superpowers/specs/2026-03-29-setup-sh-audit-report.md`.

**Findings persistence:** Each audit task (2-5) writes its findings directly into the report file under the appropriate section heading. Task 6 reads and synthesizes what Tasks 2-5 wrote. ShellCheck results from Task 1 are included in each audit task's prompt so agents can deduplicate against manual findings.

**Tech Stack:** bash, ShellCheck, manual code review

**Spec:** `docs/superpowers/specs/2026-03-29-setup-sh-audit-design.md`

---

## File Structure

| File | Action | Purpose |
|------|--------|---------|
| `setup.sh` | Read | Audit target (876 lines) |
| `.claude/rules/bash.md` | Read | Convention reference |
| `docs/superpowers/specs/2026-03-29-setup-sh-audit-design.md` | Read | Audit spec |
| `docs/superpowers/specs/2026-03-29-setup-sh-audit-report.md` | Create | Final audit report |

---

### Task 1: Collect Automated Evidence

**Files:**
- Read: `setup.sh`
- Read: `.claude/rules/bash.md`

- [ ] **Step 1: Run ShellCheck and capture full output**

Run:
```bash
shellcheck setup.sh 2>&1 || true
```

Record all findings (code, severity, line, message).

- [ ] **Step 1b: Verify syntax**

Run:
```bash
bash -n setup.sh && echo "PASS" || echo "FAIL"
```

Record pass/fail.

- [ ] **Step 2: Check line lengths**

Run:
```bash
awk 'length > 79 { printf "L%d (%d chars): %s\n", NR, length, $0 }' setup.sh
```

Record any lines exceeding 79 characters.

- [ ] **Step 3: Run bash.md compliance checklist**

Evaluate each of the 21 items from the spec's Appendix against `setup.sh`. Record pass/fail with line evidence for failures.

- [ ] **Step 4: Commit evidence summary**

No commit needed — evidence feeds directly into audit tasks.

---

### Task 2: Audit Sections 1-4 (Foundation)

**Scope:** Bootstrap checks (21-51), Constants (53-89), Error handling (91-107), Logging (109-152)
**Lines:** 129 total

**Input:** ShellCheck findings for lines 1-152 (from Task 1), passed via prompt context.

**Files:**
- Read: `setup.sh:1-152`

- [ ] **Step 1: Audit Section 1 — Bootstrap checks (L21-51)**

Evaluate across all 7 dimensions. Focus areas:
- Correctness: Does the bash version check logic handle all edge cases (e.g., bash 5.x)?
- Style: Line length, delimiter usage
- Comments: Does the sed/heredoc error message pattern follow conventions?
- Security: Is stderr redirection correct?

Record each finding as: `[Severity] [Dimension] [Line(s)] [Description] [Fix]`
Assign qualitative rating (Strong/Good/Needs attention/Needs improvement).

- [ ] **Step 2: Audit Section 2 — Constants (L53-89)**

Focus areas:
- Rule compliance: Are all constants `readonly`? UPPER_SNAKE naming?
- Correctness: Is `WORKSPACE_DIR` resolution robust (symlinks, spaces in paths)?
- Comments: Does line 75-80 comment explain WHY clearly?
- Security: Any sensitive values exposed?
- Feature completeness: Are `NETWORK_TIMEOUT` and `LINE_WIDTH` the only needed constants?

- [ ] **Step 3: Audit Section 3 — Error handling (L91-107)**

Focus areas:
- Correctness: Does `_err_handler` capture the right exit code before printing?
- Rule compliance: ERR trap pattern match bash.md template?
- Professionalism: Are color variables appropriately scoped?

- [ ] **Step 4: Audit Section 4 — Logging (L109-152)**

Focus areas:
- Correctness: Does `confirm()` handle all input cases (empty, EOF, Y/N)?
- Style: printf format string — SC2059 violations noted by ShellCheck
- Security: Any injection risk in format strings?
- Feature completeness: Missing log levels? Dead code?
- Professionalism: Single responsibility per log function?

- [ ] **Step 5: Record all findings for Sections 1-4**

Compile findings table and section ratings.

---

### Task 3: Audit Sections 5-8 (Core Logic)

**Scope:** Package path helpers (154-211), Stow state queries (213-287), Conflict resolution (289-324), Stow operations (326-426)
**Lines:** 270 total

**Input:** ShellCheck findings for lines 153-426 (from Task 1), passed via prompt context.

**Files:**
- Read: `setup.sh:153-426`

- [ ] **Step 1: Audit Section 5 — Package path helpers (L154-211)**

Focus areas:
- Correctness: Does `validate_pkgs` nameref work correctly with all edge cases?
- Rule compliance: Separate declaration/assignment for command substitution?
- Security: Is the nameref `_out` variable name collision-safe?
- Professionalism: Single responsibility of `pkg_category`, `pkg_name`, `pkg_stow_dir`?

- [ ] **Step 2: Audit Section 6 — Stow state queries (L213-287)**

Focus areas:
- Correctness: Does `stow_targets` regex correctly parse stow output?
- Style: SC2295 — unquoted expansions inside `${..}`
- Security: Is `find` usage safe (no unsanitized paths)?
- Professionalism: Is `stow_links` doing too much (tree walking + display)?

- [ ] **Step 3: Audit Section 7 — Conflict resolution (L289-324)**

Focus areas:
- Security: `rm -rf` usage — is it appropriately guarded?
- Correctness: Does `_remove_stow_conflicts` handle all target types correctly?
- Feature completeness: Is DRY_RUN mode consistent with actual behavior?

- [ ] **Step 4: Audit Section 8 — Stow operations (L326-426)**

Focus areas:
- Correctness: Does `_stow_exec` handle all stow exit codes?
- Security: Pre-creating `~/.config` — is this safe?
- Rule compliance: Short-circuit operators used as flow control?
- Professionalism: Are `stow_package`, `restow_package`, `unstow_package` well-separated?

- [ ] **Step 5: Record all findings for Sections 5-8**

Compile findings table and section ratings.

---

### Task 4: Audit Sections 9-10 (User-Facing)

**Scope:** Prerequisites (428-554), Commands (556-769)
**Lines:** 341 total — the largest and most complex group

**Input:** ShellCheck findings for lines 427-769 (from Task 1), passed via prompt context.

**Input:** ShellCheck findings for lines 427-769 (from Task 1), passed via prompt context.

**Files:**
- Read: `setup.sh:427-769`

- [ ] **Step 1: Audit Section 9 — Prerequisites (L428-554)**

Focus areas:
- Security: `curl | bash` pattern (L465-466) — is the safety comment adequate?
- Security: `eval "$(brew shellenv)"` (L475) — justified and safe?
- Correctness: Does `ensure_prerequisites` install/check mode work correctly?
- Rule compliance: `_has_*` functions — short-circuit operator usage?
- Feature completeness: Are all error paths handled?
- Professionalism: Single responsibility of `_install_*` functions?

- [ ] **Step 2: Audit Section 10 — Commands (L556-769)**

Focus areas:
- Correctness: Does `cmd_install` handle all flag combinations correctly?
- Correctness: Does `cmd_uninstall` validate packages before unstowing?
- Rule compliance: Nesting depth within command functions — max 3 levels?
- Feature completeness: Missing flags or commands?
- Style: Long functions — should they be decomposed?
- Comments: Are the command doc comments (L560-563, L646-648) accurate?

- [ ] **Step 3: Record all findings for Sections 9-10**

Compile findings table and section ratings.

---

### Task 5: Audit Sections 11-13 (Cleanup)

**Scope:** Internal helpers (771-801), Help (803-849), Entry point (851-876)
**Lines:** 104 total

**Input:** ShellCheck findings for lines 770-876 (from Task 1), passed via prompt context.

**Files:**
- Read: `setup.sh:770-876`

- [ ] **Step 1: Audit Section 11 — Internal helpers (L771-801)**

Focus areas:
- Security: `_offer_shell_switch` — `chsh -s` and `sudo tee -a /etc/shells` advisory
- Correctness: Is the zsh detection logic comprehensive?
- Security: Is `command -v zsh` result used safely in `chsh`?
- Feature completeness: What if zsh is installed but not in `/etc/shells`?

- [ ] **Step 2: Audit Section 12 — Help (L803-849)**

Focus areas:
- Correctness: Does help text match actual command/flag behavior?
- Style: Line length within heredoc
- Feature completeness: Missing examples or missing flags documented?

- [ ] **Step 3: Audit Section 13 — Entry point (L851-876)**

Focus areas:
- Rule compliance: `main()` pattern matches bash.md template?
- Correctness: Does dispatch handle all edge cases?
- Professionalism: Is the entry point clean and minimal?

- [ ] **Step 4: Record all findings for Sections 11-13**

Compile findings table and section ratings.

---

### Task 6: Synthesize and Write Report

**Files:**
- Create: `docs/superpowers/specs/2026-03-29-setup-sh-audit-report.md`

- [ ] **Step 1: Deduplicate findings**

Where ShellCheck and manual review identified the same issue for the same line,
merge into a single finding citing both evidence sources. Ensure findings count
reflects unique issues.

- [ ] **Step 2: Determine overall assessment**

Overall rating = worst section rating (per spec). Count findings by severity.

- [ ] **Step 3: Identify cross-cutting concerns**

Review all findings for patterns spanning sections. Common themes:
- Repeated style violations across multiple sections
- Systematic rule compliance gaps
- Consistent comment quality patterns

- [ ] **Step 3: Write prioritized recommendations**

Group findings into:
- Must-fix (Critical + High)
- Should-fix (Medium)
- Nice-to-have (Low + Info)

Each recommendation references specific findings by section and line.

- [ ] **Step 4: Write the full report**

Follow the report structure from the spec:
1. Executive Summary
2. Audit Methodology
3. Section-by-Section Findings (all 13 sections)
4. Cross-Cutting Concerns
5. Prioritized Recommendations
6. Appendix (ShellCheck output, compliance checklist)

- [ ] **Step 5: Commit the report**

```bash
git add docs/superpowers/specs/2026-03-29-setup-sh-audit-report.md
git commit -m "docs: add setup.sh comprehensive audit report"
```
