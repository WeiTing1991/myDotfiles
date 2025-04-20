local M = {}

local spell_words = {}
for word in io.open(vim.fn.stdpath "config" .. "/spell/en.utf-8.add", "r"):lines() do
  table.insert(spell_words, word)
end

M = {
  -- lua
  ["lua_ls"] = {
    settings = {
      Lua = {
        completion = {
          callSnippet = "Replace",
        },
        diagnostics = {
          globals = { "vim", "it", "describe", "before_each", "after_each" },
          -- disable = { "missing-fields" },
        },
      },
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
        },
      },
    },
  },

  -- js/ts/css/html
  -- Switched to ts_ls tool
  ["ts_ls"] = {},
  ["cssls"] = {
    cmd = { "css-languageserver", "--stdio" },
    filetypes = { "css", "scss", "less", "tsx", "jsx" },
  },
  ["tailwindcss"] = {},
  ["html"] = {
    filetypes = { "html", "templ" },
  },
  ["bashls"] = {},



  ["yamlls"] = {},
  ["taplo"] = {},
  ["dockerls"] = {},
  ["docker_compose_language_service"] = {},


  -- c/c++
  ["clangd"] = {
    cmd = {
      "clangd",
      "--clang-tidy",
      "--log=verbose",
      "--enable-config",
      -- "--compile-commands-dir=" .. vim.fn.getcwd() .. "/VCPKG/buildtrees/pkgconf/x64-windows-dbg",
    },
    root_dir = function()
      return vim.fn.getcwd()
    end,
    capabilities = {
      offsetEncoding = { "utf-16" },
    },
  },


  -- -- HAVE to install go global
  -- -- go
  -- gopls = {
  --   settings = {
  --     gopls = {
  --       analyses = {
  --         unusedparams = true,
  --       },
  --       staticcheck = true,
  --       gofumpt = true,
  --       semanticTokens = {
  --         enable = true, -- Enable semantic tokens
  --       },
  --     },
  --   },
  -- },
}

return M
