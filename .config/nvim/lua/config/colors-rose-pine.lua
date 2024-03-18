local opts = {}

opts = {
  variant = "moon", -- auto, main, moon, or dawn
  dark_variant = "main", -- main, moon, or dawn
  dim_inactive_windows = false,
  extend_background_behind_borders = true,
  bold_vert_split = true,

  enable = {
    terminal = true,
    legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
    migrations = true, -- Handle deprecated options automatically
  },

  styles = {
    bold = true,
    italic = false,
    transparency = true,
  },

  groups = {
    comment = "muted",
    punctuation = "subtle",
    border = "Text",
    link = "iris",

    error = "love",
    hint = "iris",
    info = "foam",
    note = "pine",
    todo = "rose",
    warn = "gold",

    git_add = "foam",
    git_change = "rose",
    git_delete = "love",
    git_dirty = "rose",
    git_ignore = "muted",
    git_merge = "iris",
    git_rename = "pine",
    git_stage = "iris",
    git_text = "rose",
    git_untracked = "subtle",

    h1 = "iris",
    h2 = "foam",
    h3 = "rose",
    h4 = "gold",
    h5 = "pine",
    h6 = "foam",
  },

  highlight_groups = {
    -- treesitter
    -- Comment = { fg = "foam" },
    -- VertSplit = { fg = "muted", bg = "muted" },
    -- for treesitter highlights, they start with an @, so you'll have to do something like:
    ["String"] = { fg = "Highlight Low" },
  },
}

require("rose-pine").setup(opts)

-- Rose Pine
-- #191724	rgb(25, 23, 36)	hsl(249deg, 22%, 12%)
-- Surface
-- #1f1d2e	rgb(31, 29, 46)	hsl(247deg, 23%, 15%)
-- Overlay
-- #26233a	rgb(38, 35, 58)	hsl(248deg, 25%, 18%)
-- Muted
-- #6e6a86	rgb(110, 106, 134)	hsl(249deg, 12%, 47%)
-- Subtle
-- #908caa	rgb(144, 140, 170)	hsl(248deg, 15%, 61%)
-- Text
-- #e0def4	rgb(224, 222, 244)	hsl(245deg, 50%, 91%)
-- Love
-- #eb6f92	rgb(235, 111, 146)	hsl(343deg, 76%, 68%)
-- Gold
-- #f6c177	rgb(246, 193, 119)	hsl(35deg, 88%, 72%)
-- Rose
-- #ebbcba	rgb(235, 188, 186)	hsl(2deg, 55%, 83%)
-- Pine
-- #31748f	rgb(49, 116, 143)	hsl(197deg, 49%, 38%)
-- Foam
-- #9ccfd8	rgb(156, 207, 216)	hsl(189deg, 43%, 73%)
-- Iris
-- #c4a7e7	rgb(196, 167, 231)	hsl(267deg, 57%, 78%)
-- Highlight Low
-- #21202e	rgb(33, 32, 46)	hsl(244deg, 18%, 15%)
-- Highlight Med
-- #403d52	rgb(64, 61, 82)	hsl(249deg, 15%, 28%)
-- Highlight High
-- #524f67	rgb(82, 79, 103)	hsl(248deg, 13%, 36%)
