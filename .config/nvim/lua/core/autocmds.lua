local autocmd = vim.api.nvim_create_autocmd

autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = "*",
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd([[ %s/\s\+$//e ]])
    end
  end,
})

autocmd("BufEnter", {
  group = vim.api.nvim_create_augroup("wtc/disable_automatic_comment", { clear = true }),
  pattern = "*",
  callback = function()
    vim.opt_local.formatoptions:remove({ "c", "r", "o" })
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
autocmd({ "BufRead", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("wtc/python", { clear = true }),
  pattern = "*.py",
  callback = function()
    vim.opt_local.conceallevel = 0
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
  end,
})

--[[ C/C++ ]]
autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wtc/cpp", { clear = true }),
  pattern = { "cpp", "c", "h" },
  callback = function()
    vim.opt_local.textwidth = 100
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2

    vim.keymap.set("n", "<leader>h", "<cmd>ClangdSwitchSourceHeader<cr>",
      { buffer = true, desc = "Switch between source and header" })
  end,
})

--[[ json ]]
autocmd({ "BufRead", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("wtc/json", { clear = true }),
  pattern = "*.json",
  callback = function()
    vim.opt_local.conceallevel = 0
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.textwidth = 150
  end,
})

--[[ Markdown / YAML / Text ]]
autocmd({ "BufRead", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("wtc/markdown", { clear = true }),
  pattern = { "*.md", "*.yaml", "*.yml", "*.txt" },
  callback = function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.textwidth = 120
    vim.opt.foldlevel = 99

    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.number = true
    vim.opt_local.relativenumber = false
    vim.opt_local.conceallevel = 0
    require("snacks.indent").disable()

    vim.keymap.set("n", "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>",
      { buffer = true, desc = "markdown preview" })
    vim.keymap.set("v", "<C-b>", 'c**<C-r>"**<Esc>', { buffer = true, desc = "Bold" })
    vim.keymap.set("v", "<C-i>", 'c*<C-r>"*<Esc>', { buffer = true, desc = "Italic" })
    vim.keymap.set("v", "<C-S>`", 'c`<C-r>"`<Esc>', { buffer = true, desc = "Inline Code" })
  end,
})

-- For Octo buffers (GitHub PRs/Issues)
autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wtc/octo", { clear = true }),
  pattern = "octo",
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
    vim.opt_local.number = true
    vim.opt_local.relativenumber = false
    vim.opt_local.conceallevel = 2
    vim.opt_local.textwidth = 120
  end,
})

--[[ C# ]]
autocmd({ "BufRead", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("wtc/csharp", { clear = true }),
  pattern = "*.cs",
  callback = function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.textwidth = 120
  end,
})
