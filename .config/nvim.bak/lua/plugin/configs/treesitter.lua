local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
  modules = {},
  ensure_installed = {
    "vim",
    "vimdoc",
    "git_config",
    "gitcommit",
    "git_rebase",
    "gitignore",
    "diff",

    "bash",
    "c",
    "cpp",
    "make",
    "cmake",
    "proto",

    "lua",
    "luadoc",
    "query",

    "python",
    "c_sharp",
    "java",

    "go",
    "gomod",
    "gowork",
    "gosum",

    "regex",
    "dockerfile",

    "toml",
    "json",
    "jsonc",
    "markdown",
    "markdown_inline",
    "latex",
    "typst",

    "javascript",
    "typescript",
    "tsx",
    "jsdoc",

    "xml",
    "html",
    "yaml",
    "toml",
    "css",
  },
  auto_install = true,
  ignore_install = {},
  sync_install = false,

  highlight = {
    enable = true,
    use_languagetree = true,
    -- disable in bigger file
    disable = function(lang, buf)
      local max_filesize = 10 * 1024 * 1024 -- 10 MB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
    -- additional_vim_regex_highlighting = { "markdown" },
    additional_vim_regex_highlighting = false,
  },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-space>",
      node_incremental = "<C-space>",
      scope_incremental = false,
      node_decremental = "<bs>",
    },
  },
  textobjects = {
    move = {
      enable = true,
      goto_next_start = { ["]f"] = "@function.outer", ["]c"] = "@class.outer", ["]a"] = "@parameter.inner" },
      goto_next_end = { ["]F"] = "@function.outer", ["]C"] = "@class.outer", ["]A"] = "@parameter.inner" },
      goto_previous_start = { ["[f"] = "@function.outer", ["[c"] = "@class.outer", ["[a"] = "@parameter.inner" },
      goto_previous_end = { ["[F"] = "@function.outer", ["[C"] = "@class.outer", ["[A"] = "@parameter.inner" },
    },
  },
}

require("treesitter-context").setup {
  enable = true,
  multiwindow = true,
  max_lines = 3,
  min_window_height = 0,
  line_numbers = true,
  multiline_threshold = 20,
  trim_scope = "outer",
  mode = "cursor",
  oeparator = "-",
  zindex = 20,
  on_attach = nil,
}

-- Set up TreesitterContext highlights
vim.api.nvim_set_hl(0, "TreesitterContext", { bg = "NONE", bold = true })
vim.api.nvim_set_hl(0, "TreesitterContextBottom", { underline = true, sp = "#ffffff", })
