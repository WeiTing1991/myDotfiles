local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
  ensure_installed = {
    "bash",
    "c",
    "cpp",
    "cmake",
    "lua",
    "json",
    "python",
    "toml",
    "markdown",
  },
  ignore_install = {},
  sync_install = false,
  auto_install = false,
  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don"t have `tree-sitter` CLI installed locally

  highight = {
    enable = true,
    additional_vim_regex_highlighting = true,
  },
  indent = { enable = true }
}
