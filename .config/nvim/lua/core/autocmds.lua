local autocmd = vim.api.nvim_create_autocmd
local set = vim.opt_local

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd([[ %s/\s\+$//e ]]) -- Trim trailing whitespace
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

autocmd({ "WinEnter", "BufEnter" }, {
  callback = function()
    vim.wo.winhl = "WinSeparator:WinSeparatorFocused"
  end,
})

autocmd({ "WinLeave", "BufLeave" }, {
  callback = function()
    vim.wo.winhl = "WinSeparator:WinSeparator"
  end,
})

--[[ python ]]
local pygroup = vim.api.nvim_create_augroup("pygroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = pygroup,
  pattern = "*.py",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
  end,
})

--[[ C/C++ ]]
local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })
autocmd("FileType", {
  group = cppgroup,
  pattern = { "cpp", "c", "h" },
  callback = function()
    vim.opt_local.textwidth = 100
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2

    vim.keymap.set("n", "<leader>h", "<cmd>ClangdSwitchSourceHeader<cr>", { desc = "Switch between source and header" })
  end,
})

--[[ json ]]
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

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = mdgroup,
  pattern = { "*.md", "*.yaml", "*.yml", "*.txt" },
  callback = function()
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
    set.textwidth = 120
    vim.opt.foldlevel = 99

    set.wrap = true
    set.spell = true
    set.number = true
    set.relativenumber = false
    set.conceallevel = 0
    require("snacks.indent").disable()

    -- vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "javascript", "json", "yaml", "vim", "lua" }

    --[[ Markdown ]]
    vim.keymap.set("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", { desc = "markdown preview" })
    vim.keymap.set("v", "<C-b>", 'c**<C-r>"**<Esc>', { desc = "Bold" })
    vim.keymap.set("v", "<C-i>", 'c*<C-r>"*<Esc>', { desc = "Italic" })
    vim.keymap.set("v", "<C-S>`", 'c`<C-r>"`<Esc>', { desc = "Inline Code" })
    -- vim.keymap.set("v", "<leader>ms", 'c~~<C-r>"~~<Esc>', { desc = "Strikethrough" })
    -- vim.keymap.set("v", "<leader>mc", "c```<C-r>/```<Esc>", { desc = "Strikethrough" })
  end,
})

-- For Octo buffers (GitHub PRs/Issues)
vim.api.nvim_create_autocmd("FileType", {
  group = mdgroup,
  pattern = "octo",
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.number = true
    vim.opt_local.relativenumber = false
    vim.opt_local.conceallevel = 2 -- Better for markdown rendering in octo
    vim.opt_local.textwidth = 120
  end,
})

--[[ c# ]]
local cSharp = vim.api.nvim_create_augroup("cSharpgroup", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
  group = cSharp,
  pattern = "*.cs",
  callback = function()
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
    set.textwidth = 120
  end,
})

-- --[[ ts/js ]]
-- local ts_funcs = require("lsp.typescriptFunc")
-- local tsgroup = vim.api.nvim_create_augroup("tsgroup", { clear = true })
-- autocmd({ "BufRead", "BufNewFile" }, {
--   group = tsgroup,
--   pattern = { "*.js", "*.ts", "*.tsx", "*.jsx" },
--   callback = function(args)
--     set.conceallevel = 0
--     set.shiftwidth = 2
--     set.tabstop = 2
--     set.softtabstop = 2
--     set.textwidth = 120
--
--     local bufnr = args.buf
--     local opts = { buffer = bufnr, noremap = true, silent = true }
--
--     vim.keymap.set('n', '<S-l>to', ts_funcs.ts_organize_imports, vim.tbl_extend('force', opts, { desc = "Organize Imports" }))
--     vim.keymap.set('n', '<S-l>tu', ts_funcs.ts_remove_unused, vim.tbl_extend('force', opts, { desc = "Remove Unused" }))
--     vim.keymap.set('n', '<S-l>ta', ts_funcs.ts_add_missing_imports, vim.tbl_extend('force', opts, { desc = "Add Missing Imports" }))
--     vim.keymap.set('n', '<S-l>tf', ts_funcs.ts_fix_all, vim.tbl_extend('force', opts, { desc = "Fix All" }))
--     vim.keymap.set('n', '<S-l>tr', ts_funcs.ts_remove_unused_imports, vim.tbl_extend('force', opts, { desc = "Remove Unused Imports" }))
--     vim.keymap.set('n', '<S-l>ts', ts_funcs.ts_sort_imports, vim.tbl_extend('force', opts, { desc = "Sort Imports" }))
--
--     -- Add LazyVim's enhanced keymaps
--     vim.keymap.set('n', 'gD', ts_funcs.ts_goto_source_definition, vim.tbl_extend('force', opts, { desc = "Go to Source Definition" }))
--     vim.keymap.set('n', 'gR', ts_funcs.ts_find_file_references, vim.tbl_extend('force', opts, { desc = "Find File References" }))
--
--   end
-- })
-- local autocmd = vim.api.nvim_create_autocmd
-- autocmd("BufWinEnter", {
--   group = vim.api.nvim_create_augroup("wt/fugitive", {}),
--   pattern = "*",
--   callback = function()
--     if vim.bo.ft ~= "fugitive" then
--       return
--     end
--     local bufnr = vim.api.nvim_get_current_buf()
--     local opts = { buffer = bufnr, remap = false }
--     vim.keymap.set("n", "<leader>p", function()
--       vim.cmd.Git("push")
--     end, opts)
--     -- rebase always
--     vim.keymap.set("n", "<leader>P", function()
--       vim.cmd.Git({ "pull", "--rebase" })
--     end, opts)
--     vim.keymap.set("n", "<leader>t", ":Git push -u origin ", opts)
--   end,
-- })
-- map("n", "gu", "<cmd>diffget //2<CR>", { desc = "Diff get" })
-- map("n", "gh", "<cmd>diffget //3<CR>", { desc = "Diff get" })
