local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
  modules = {},
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
    "luadoc",
    "python",
    "java",

    "javascript",
    "typescript",
    "jsdoc",

    "go",
    "gomod",
    "gowork",
    "gosum",

    "regex",
    "dockerfile",
    "gitignore",

    "toml",
    "json",
    "json5",
    "jsonc",
    "markdown",
    "markdown_inline",
    "latex",

    "xml",
    "html",
    "yaml",
    "toml",
    "css",
    "templ",

  },
  auto_install = true,
  ignore_install = {},
  sync_install = false,

  highlight = {
    enable = true,
    use_languagetree = true,

    -- disable in bigger file
    disable = function(lang, buf)
      local max_filesize = 10 * 1024 * 1024 -- 2 MB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
    -- additional_vim_regex_highlighting = { "markdown" },
    additional_vim_regex_highlighting = false,
  },
  indent = { enable = true },
}
