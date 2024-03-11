local luacheck = require("efmls-configs.linters.luacheck")

local languages = {
  lua = { luacheck },
}

local efmls_config = {
  filetypes = vim.tbl_keys(languages),
  settings = {
    rootMarkers = { ".git/" },
    languages = languages,
  },
  init_options = {
    documentFormatting = true,
    documentRangeFormatting = true,
  },
}

require("lspconfig").efm.setup(vim.tbl_extend("force", efmls_config, {
  -- on_attach = on_attach,
  -- capabilities = capabilities,
}))
