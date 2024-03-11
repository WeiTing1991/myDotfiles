local M = {

  "nvim-treesitter/nvim-treesitter",
  lazy = false,
  --event = { "BufReadPre", "BufNewFile" },
  build = ":TSUpdate",
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
  },
}

function M.config()
  require("nvim-treesitter.configs").setup({
    --check the https://github.com/nvim-treesitter/nvim-treesitter
    ensure_installed = {
      "bash",
      "c",
      "diff",
      "html",
      "javascript",
      "jsdoc",
      "json",
      "jsonc",
      "lua",
      "luadoc",
      "luap",
      "markdown",
      "markdown_inline",
      "python",
      "query",
      "regex",
      "toml",
      "tsx",
      "typescript",
      "vim",
      "vimdoc",
      "yaml",
      "cpp",
      "rust",
      "cmake",
      "css",
      "dockerfile",
      "gitignore",
      "toml",
      "xml",
      "java",
    },
    ignore_install = {},
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don"t have `tree-sitter` CLI installed locally
    auto_install = true,

    auto_tags = {
      enable = true,
    },

    Modules = {},
    indent = {
      enable = true,
    },
    autopairs = { enable = true },
    highight = {
      -- `false` will disable the whole extension
      enable = true,
      additional_vim_regex_highlighting = { "java" },
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<leader>gn", -- set to `false` to disable one of the mappings
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

        patterns = {
          java = {
            "public",
            "private",
            "void",
            "protected",
            "interface",
            "enum",
            "do",
          },
        },
        exact_patterns = {
          -- Example for a specific filetype with Lua patterns
          -- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
          -- exactly match "impl_item" only)
          -- rust = true,
          java = true,
        },
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["at"] = "@class.outer",
          ["it"] = "@class.inner",
          ["ac"] = "@call.outer",
          ["ic"] = "@call.inner",
          ["aa"] = "@parameter.outer",
          ["ia"] = "@parameter.inner",
          ["al"] = "@loop.outer",
          ["il"] = "@loop.inner",
          ["ai"] = "@conditional.outer",
          ["ii"] = "@conditional.inner",
          ["a/"] = "@comment.outer",
          ["i/"] = "@comment.inner",
          ["ab"] = "@block.outer",
          ["ib"] = "@block.inner",
          ["as"] = "@statement.outer",
          ["is"] = "@scopename.inner",
          ["aA"] = "@attribute.outer",
          ["iA"] = "@attribute.inner",
          ["aF"] = "@frame.outer",
          ["iF"] = "@frame.inner",
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
  })
end

--    - Incremental selection: Included, see :help nvim-treesitter-incremental-selection-mod
--    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
--    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects

-- the setting template for new lanuague.
--local treesitter_parser_config = require("nvim-treesitter.parsers").get_parser_configs()
--treesitter_parser_config.templ = {
--    install_info = {
--        url = "https://github.com/vrischmann/tree-sitter-templ.git",
--        files = {"src/parser.c", "src/scanner.c"},
--        branch = "master",
--    },
--}
--vim.treesitter.language.register("templ", "templ")

return M
