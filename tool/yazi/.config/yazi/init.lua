-- init.lua -*- mode: lua; -*-
-- Time-stamp: <2026-03-25 00:00:00 Tue by zhengyu.li>
-- =============================================================================
-- Yazi - Initialization Script
--
-- Usage:    Automatically loaded by yazi on startup
--
-- Plugins (installed via ya pkg):
--   - full-border:   Complete frame around all panels
--   - git:           Git status indicators
--   - smart-enter:   Enter directory or open file (key binding)
--   - smart-filter:  Interactive filtering (key binding)
--   - toggle-pane:   Toggle preview pane (key binding)
--   - zoom:          Zoom preview pane (key binding)
--
-- References:
--   1. https://yazi-rs.github.io/docs/configuration/init/
--   2. https://yazi-rs.github.io/docs/tips/
-- =============================================================================

-- -----------------------------------------------------------------------------
-- UI Plugins
-- -----------------------------------------------------------------------------

-- Enable full border plugin for complete frame around all panels
require("full-border"):setup()

-- -----------------------------------------------------------------------------
-- Git Plugin
-- -----------------------------------------------------------------------------

-- Configure git status indicators with catppuccin-mocha colors
th.git = th.git or {}
th.git.modified = ui.Style():fg("#f9e2af")
th.git.added = ui.Style():fg("#a6e3a1")
th.git.untracked = ui.Style():fg("#f5c2e7")
th.git.ignored = ui.Style():fg("#6c7086")
th.git.deleted = ui.Style():fg("#f38ba8")
th.git.updated = ui.Style():fg("#89b4fa")

th.git.modified_sign = "M"
th.git.added_sign = "A"
th.git.untracked_sign = "?"
th.git.ignored_sign = "!"
th.git.deleted_sign = "D"
th.git.updated_sign = "U"

-- Enable git plugin with order for linemode display
require("git"):setup {
	order = 1500,
}
