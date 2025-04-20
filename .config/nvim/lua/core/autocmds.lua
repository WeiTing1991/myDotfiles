local autocmd = vim.api.nvim_create_autocmd

vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("wtc/trim_trailing_whitespace", { clear = true }),
  pattern = { "*" }, -- Apply to all files
  callback = function()
    if vim.bo.filetype ~= "markdown" then
      vim.cmd [[ %s/\s\+$//e ]] -- Trim trailing whitespace
    end
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("wtc/yank_highlight", { clear = true }),
  callback = function()
    vim.hl.on_yank { higroup = "Visual", priority = 250 }
  end,
})

vim.api.nvim_create_autocmd("VimEnter", {
  group = vim.api.nvim_create_augroup("wtc/winbar", { clear = true }),
  pattern = { "*" },
  callback = function()
    vim.api.nvim_set_hl(0, "WinBar", { bg = "#303030" })
    vim.api.nvim_set_hl(0, "WinBarNC", { bg = "#1c1c1c" })
    vim.api.nvim_set_hl(0, "WinSeparator", { fg = "#ffffff" })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wtc/close_with_q", { clear = true }),
  pattern = {
    "git",
    "diff",
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
