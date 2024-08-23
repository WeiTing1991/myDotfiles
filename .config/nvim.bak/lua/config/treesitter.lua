local treesitter = require "nvim-treesitter.configs"

treesitter.setup {
  TSConfig = {},
  modules = {},
  --check the https://github.com/nvim-treesitter/nvim-treesitter
  ensure_installed = {
    "go",
    "gomod",
    "gowork",
    "gosum",
    "bash",
    "cpp",
    "c",
    "java",
    "json",
    "lua",
    "luadoc",
    "markdown",
    "markdown_inline",
    "python",
    "regex",
    "vim",
    "vimdoc",
    "yaml",
    "rust",
    "cmake",
    "make",
    "dockerfile",
    "gitignore",
    "xml",
    "html",
    "css",
    "templ",
  },
  ignore_install = {},
  sync_install = false,
  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don"t have `tree-sitter` CLI installed locally
  auto_install = true,
  auto_tags = {
    enable = true,
    enable_rename = true,
    enable_close = true,
    enable_close_on_slash = true,
    filetypes = { "html", "xml", "htmx", "templ"},
  },
  indent = {
    enable = true,
  },
  autopairs = {
    enable = true,
  },
  highight = {
    -- `false` will disable the whole extension
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<leader>gnn", -- set to `false` to disable one of the mappings
      node_incremental = "<leader>grn",
      scope_incremental = false,
      node_decremental = "<leader>grm",
    },
  },
  --https://github.com/nvim-treesitter/nvim-treesitter-textobjects
  textobjects = {
    select = {
      enable = true,
      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
        -- You can optionally set descriptions to the mappings (used in the desc parameter of
        -- nvim_buf_set_keymap) which plugins like which-key display
        -- You can also use captures from other query groups like `locals.scm`
        ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
      },
      -- You can choose the select mode (default is charwise 'v')
      --
      -- Can also be a function which gets passed a table with the keys
      -- * query_string: eg '@function.inner'
      -- * method: eg 'v' or 'o'
      -- and should return the mode ('v', 'V', or '<c-v>') or a table
      -- mapping query_strings to modes.
      selection_modes = {
        ["@parameter.outer"] = "v", -- charwise
        ["@function.outer"] = "V", -- linewise
        ["@class.outer"] = "<c-v>", -- blockwise
      },
      -- If you set this to `true` (default is `false`) then any textobject is
      -- extended to include preceding or succeeding whitespace. Succeeding
      -- whitespace has priority in order to act similarly to eg the built-in
      -- `ap`.
      --
      -- Can also be a function which gets passed a table with the keys
      -- * query_string: eg '@function.inner'
      -- * selection_mode: eg 'v'
      -- and should return true or false
      include_surrounding_whitespace = true,
    },
  },
}
-- the setting template for new lanuague.
-- local treesitter_parser_config = require("nvim-treesitter.parsers").get_parser_configs()
-- treesitter_parser_config.go = {
--   install_info = {
--     url = "https://github.com/tree-sitter/tree-sitter-go",
--     files = { "src/parser.c", "src/scanner.c" },
--     branch = "master",
--   },
-- }
-- vim.treesitter.language.register("go", "go")
