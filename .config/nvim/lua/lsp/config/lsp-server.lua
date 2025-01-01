-- lsp server
local M = {}

local spell_words = {}
for word in io.open(vim.fn.stdpath "config" .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

M = {
  -- lua
  lua_ls = {
    -- settings = {
    --   Lua = {
    --     diagnostics = {
    --       globals = { "vim", "it", "describe", "before_each", "after_each" },
    --       disable = { "missing-fields" },
    --     },
    --     completion = {
    --       callSnippet = "Replace",
    --     },
    --   },
    -- },
  },

  --ltex
  -- ltex = {
  --   settings = {
  --     ltex = {
  --       enabled = false,
  --       language = "en-US",
  --       dictionary = {
  --         ["en-US"] = spell_words,
  --       },
  --     },
  --   },
  -- },
  marksman = {},

  -- bashls = {},
  -- jsonls = {},
  -- html = {},
  -- cssls = {},
  --
  -- -- python
  -- pyright = {
  --   settings = {
  --     pyright = {
  --       disableOrganizeImports = true,
  --     },
  --     python = {
  --       analysis = {
  --         typeCheckingMode = "basic",
  --       },
  --     },
  --   },
  -- },
  -- basedpyright = {
  --   settings = {
  --     basedpyright = {
  --       typeCheckingMode = "off",
  --       logLevel = "error",
  --     },
  --   },
  -- },
  -- ruff = {},
  --
  -- -- c/ c++
  -- clangd = {
  --   cmd = {
  --     "clangd",
  --     "--clang-tidy",
  --     "--log=verbose",
  --     "--enable-config",
  --     -- "--compile-commands-dir=" .. vim.fn.getcwd() .. "/VCPKG/buildtrees/pkgconf/x64-windows-dbg",
  --   },
  --   root_dir = function()
  --     return vim.fn.getcwd()
  --   end,
  --   capabilities = {
  --     offsetEncoding = { "utf-16" },
  --   },
  -- },
}

return M
