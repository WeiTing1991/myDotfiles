
local null_ls = require "null-ls"

-- NOTE: https://github.com/nvimtools/none-ls.nvim
local formatting = null_ls.builtins.formatting

require("null-ls").setup {
  debug = false,
  -- on_init = function(new_client, _)
  --   new_client.offset_encoding = "utf-16"
  -- end,
  sources = {

    -- spell
    null_ls.builtins.completion.spell,

    -- lua
    -- formating
    formatting.stylua,

    formatting.shfmt.with {
      filetypes = { "sh", "bash", "zsh" },
    },

    -- js/ts
    -- formatting.prettierd.with {
    --   command = "prettierd",
    --   filetyes = {
    --     "javascript", -- check also ;
    --     "typescript",
    --     "css",
    --     "html",
    --     "json",
    --     "jsonc",
    --     "yaml",
    --     "markdown",
    --   },
    --   extra_filetypes = { "toml" },
    -- },

    formatting.biome.with {
      command = "biome",
      filetypes = {
        "javascript",
        "typescript",
        "javascriptreact",
        "typescriptreact",
        "css",
        "html",
        "json",
        "jsonc",
        "yaml",
        "markdown",
      },
    },

    -- require("none-ls.diagnostics.eslint_d").with {
    --   args = {
    --     "--no-warn-ignored", -- <-- this is the key argument
    --     "--format",
    --     "json",
    --     "--stdin",
    --     "--stdin-filename",
    --     function()
    --       return vim.api.nvim_buf_get_name(0)
    --     end,
    --   },
    --   filetypes = {
    --     "javascript",
    --     "typescript",
    --   },
    -- },

    -- python
    -- formating
    require("none-ls.diagnostics.ruff").with {
      filetypes = { "python" },
    },

    require("none-ls.formatting.ruff").with {
      filetypes = { "python" },
    },

    -- formatting.isort.with {
    --   filetypes = { "python" },
    -- },

    -- formatting.black.with {
    --   filetypes = { "python" },
    --   extra_arges = { "--fast" },
    -- },

  },
}
