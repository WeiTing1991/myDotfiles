-- NOTE: https://github.com/yioneko/vtsls
--[[ VTSLS TypeScript Functions
- rename file
- refactoring
- go to source definition
- organize imports
- remove unused imports
- add missing imports
- fix all diagnostics
- file references
- restart server
]]
local M = {}
local M = {}

-- LazyVim's exact code actions (these work!)
M.ts_organize_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.organizeImports" }, -- Remove .ts suffix
      diagnostics = {},
    },
  })
end

M.ts_remove_unused = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.removeUnused.ts" },
      diagnostics = {},
    },
  })
end

M.ts_add_missing_imports = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.addMissingImports.ts" },
      diagnostics = {},
    },
  })
end

M.ts_fix_all = function()
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.fixAll.ts" },
      diagnostics = {},
    },
  })
end

M.ts_remove_unused_imports = function()
  -- Same as remove_unused for vtsls
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.removeUnused.ts" },
      diagnostics = {},
    },
  })
end

M.ts_sort_imports = function()
  -- vtsls organizes imports (which includes sorting)
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { "source.organizeImports" },
      diagnostics = {},
    },
  })
end

-- LazyVim's enhanced functions
M.ts_goto_source_definition = function()
  local params = vim.lsp.util.make_position_params()
  vim.lsp.buf.execute_command({
    command = "typescript.goToSourceDefinition",
    arguments = { params.textDocument.uri, params.position },
  })
end

M.ts_find_file_references = function()
  vim.lsp.buf.execute_command({
    command = "typescript.findAllFileReferences",
    arguments = { vim.uri_from_bufnr(0) },
  })
end

M.ts_select_version = function()
  vim.lsp.buf.execute_command({
    command = "typescript.selectTypeScriptVersion"
  })
end

return M
