-- NOTE: https://github.com/nvimtools/none-ls.nvim/blob/main/doc/BUILTINS.md#hover
local null_ls = require("null-ls")
-- local null_ls_utils = require("null-ls.utils")
local formatting = null_ls.builtins.formatting
-- local diagnostics = null_ls.builtins.diagnostics

local filetypesTS = {
  "javascript",
  "typescript",
  "javascriptreact",
  "typescriptreact",
}

require("null-ls").setup({
  debug = false,
  sources = {

    null_ls.builtins.completion.spell,

    -- Github action
    null_ls.builtins.diagnostics.actionlint,

    -- formating
    -- lua
    formatting.stylua,

    -- prettier
    formatting.prettier.with({
      filetyes = {
        "json",
        "jsonc",
        "yaml",
        "markdown",
        "html",
      },
      extra_filetypes = { "toml" },
      disabled_filetypes = filetypesTS,
    }),

    formatting.shfmt.with({
      filetypes = { "sh", "bash", "zsh" },
    }),

    -- formatting.xmllint,

    -- c/cpp/cmake
    formatting.clang_format.with({
      filetypes = { "c", "cpp", "objc", "objcpp", "h", "hpp" },
    }),

    -- python
    require("none-ls.formatting.ruff").with({
      filetypes = { "python" },
    }),
    -- require("none-ls.diagnostics.ruff").with({
    --   filetypes = { "python" },
    -- }),

    -- CSharp
    formatting.csharpier,

    -- --    -- js/ts
    --   formatting.biome.with({
    --     command = "biome",
    --     filetypes = filetypesTS,
    --   }),
    -- },
    -- --    -- require("none-ls.diagnostics.eslint_d"),
    -- --    require("none-ls.diagnostics.eslint_d").with {
    -- --      args = {
    -- --        "--no-warn-ignored", -- <-- this is the key argument
    -- --        "--format", "json",
    -- --        "--stdin",
    -- --        "--stdin-filename",
    -- --        function()
    -- --          return vim.api.nvim_buf_get_name(0)
    -- --        end,
    -- --      },
    -- --      filetypes = {
    -- --        "javascript",
    -- --        "typescript",
    -- --      },
  },
})
