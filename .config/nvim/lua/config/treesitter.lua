local treesitter = require "nvim-treesitter.configs"

treesitter.setup ({
  ensure_installed = {
    "vim",
    "vimdoc",

    "bash",
    "c",
    "cpp",
    "diff",
    "make",
    "cmake",
    "lua",
    "python",
    "java",

    "go",
    -- "gomod",
    -- "gowork",
    -- "gosum",

    "regex",
    "dockerfile",
    "gitignore",

    "toml",
    "json",
    "markdown",
    "markdown_inline",
    "latex",

    "xml",
    "html",
    "css",
    "templ",

  },
  auto_install = true,
  -- ignore_install = {},
  sync_install = false,

  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false
  },

  indent = { enable = true },
})
