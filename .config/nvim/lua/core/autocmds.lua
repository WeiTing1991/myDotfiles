local autocmd = vim.api.nvim_create_autocmd
local set = vim.opt_local

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd [[ %s/\s\+$//e ]] -- Trim trailing whitespace
    end
  end,
})

autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("wtc/yank_highlight", { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

-- autocmd({ "bufenter", "bufwinenter" }, {
--   group = vim.api.nvim_create_augroup("wtc/editor", { clear = true }),
--   pattern = { "*" },
--   callback = function()
--     vim.cmd [[set formatoptions-=c formatoptions-=r formatoptions-=o]]
--   end,
-- })

autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wt-local_detach_fold", { clear = true }),
  pattern = {
    "alpha",
    "snacks_*",
    "help",
    "dashboard",
    "snacks_dashboard",
  },
  callback = function()
    vim.o.foldenable = false
    vim.o.foldcolumn = "0"
  end,
})

autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wtc/close_with_q", { clear = true }),
  pattern = {
    "git",
    "diffview",
    "help",
    "man",
    "qf",
    "query",
    "scratch",
  },
  callback = function(args)
    vim.keymap.set("n", "q", "<cmd>quit<cr>", { buffer = args.buf })
  end,
})

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = mdgroup,
  pattern = "*.md",
  callback = function()
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150
    vim.opt.foldlevel = 99

    set.wrap = true
    set.spell = true
    set.number = false
    set.relativenumber = false
    set.conceallevel = 0
    -- vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "javascript", "json", "yaml", "vim", "lua" }
    -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
  end,
})

--[[ -- json ]]
local jsongroup = vim.api.nvim_create_augroup("jsongroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = jsongroup,
  pattern = "*.json",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150
  end,
})

--[[ -- c# ]]
local cSharp = vim.api.nvim_create_augroup("cSharpgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = cSharp,
  pattern = "*.cs",
  callback = function()
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
    set.textwidth = 120
    vim.keymap.set('n', 'gd', require('omnisharp_extended').lsp_definitions, { desc = "Goto Definition" })
  end,
})

--[[ ts/js ]]
local ts_funcs = require "lsp.typescriptFunc"
local tsgroup = vim.api.nvim_create_augroup("tsgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = tsgroup,
  pattern = { "*.js", "*.ts", "*.tsx", "*.jsx" },
  callback = function(args)
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 120

    local bufnr = args.buf
    local opts = { buffer = bufnr, noremap = true, silent = true }

    -- stylua: ignore start
    -- Set keymaps for TypeScript/JavaScript buffers
    vim.keymap.set('n', '<S-l>to', ts_funcs.ts_organize_imports, vim.tbl_extend('force', opts, { desc = "Organize Imports" }))
    vim.keymap.set('n', '<S-l>tu', ts_funcs.ts_remove_unused, vim.tbl_extend('force', opts, { desc = "Remove Unused" }))
    vim.keymap.set('n', '<S-l>ta', ts_funcs.ts_add_missing_imports, vim.tbl_extend('force', opts, { desc = "Add Missing Imports" }))
    vim.keymap.set('n', '<S-l>tf', ts_funcs.ts_fix_all, vim.tbl_extend('force', opts, { desc = "Fix All" }))
    vim.keymap.set('n', '<S-l>tr', ts_funcs.ts_remove_unused_imports, vim.tbl_extend('force', opts, { desc = "Remove Unused Imports" }))
    vim.keymap.set('n', '<S-l>ts', ts_funcs.ts_sort_imports, vim.tbl_extend('force', opts, { desc = "Sort Imports" }))

    -- Create buffer-local commands
    vim.api.nvim_buf_create_user_command(bufnr, "TSOrganizeImports", ts_funcs.ts_organize_imports, { desc = "Organize Imports" })
    vim.api.nvim_buf_create_user_command(bufnr, "TSRemoveUnused", ts_funcs.ts_remove_unused, { desc = "Remove Unused" })
    vim.api.nvim_buf_create_user_command(bufnr, "TSAddMissingImports", ts_funcs.ts_add_missing_imports, { desc = "Add Missing Imports" })
    vim.api.nvim_buf_create_user_command(bufnr, "TSFixAll", ts_funcs.ts_fix_all, { desc = "Fix All" })
    vim.api.nvim_buf_create_user_command(bufnr, "TSRemoveUnusedImports", ts_funcs.ts_remove_unused_imports, { desc = "Remove Unused Imports" })
    vim.api.nvim_buf_create_user_command(bufnr, "TSSortImports", ts_funcs.ts_sort_imports, { desc = "Sort Imports" })

    -- stylua: ignore end
  end,
})

-- local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })
-- autocmd("FileType", {
--   group = cppgroup,
--   pattern = { "cpp", "c", "h" },
--   callback = function()
--     vim.opt_local.textwidth = 100
--     vim.opt_local.tabstop = 2
--     vim.opt_local.softtabstop = 2
--     vim.opt_local.shiftwidth = 2
--     vim.opt_local.expandtab = true
--
--     vim.keymap.set("n", "<leader>h", "<cmd>ClangdSwitchSourceHeader<cr>", { desc = "Switch between source and header" })
--   end,
-- })
--
