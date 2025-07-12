-- NOTE: https://github.com/typescript-language-server/typescript-language-server


--[[ rename file
refactoring
go to source defintion ]]

local M = {}

-- TypeScript specific code actions
M.ts_organize_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.organizeImports.ts"},
      diagnostics = {},
    },
  })
end

M.ts_remove_unused = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.removeUnused.ts"},
      diagnostics = {},
    },
  })
end

M.ts_add_missing_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.addMissingImports.ts"},
      diagnostics = {},
    },
  })
end

M.ts_fix_all = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.fixAll.ts"},
      diagnostics = {},
    },
  })
end

M.ts_remove_unused_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.removeUnusedImports.ts"},
      diagnostics = {},
    },
  })
end

M.ts_sort_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = {"source.sortImports.ts"},
      diagnostics = {},
    },
  })
end

return M
