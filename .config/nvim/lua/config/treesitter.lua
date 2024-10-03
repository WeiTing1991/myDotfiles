local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
  ensure_installed = {
    "vim",
    "vimdoc",

    "bash",
    "c",
    "cpp",
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

    "xml",
    "html",
    "css",
    "templ",
  },
  ignore_install = {},
  sync_install = false,
  auto_install = true,
  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don"t have `tree-sitter` CLI installed locally

  highight = {
    enable = true,
    additional_vim_regex_highlighting = { "markdown" },
  },
  indent = { enable = true },
}
