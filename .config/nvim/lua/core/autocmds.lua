local autocmd = vim.api.nvim_create_autocmd

-- CHECK: if this work or not?

-- user event that loads after UIEnter + only if file buf is there
autocmd({ "UIEnter", "BufReadPost", "BufNewFile" }, {
  group = vim.api.nvim_create_augroup("NvFilePost", { clear = true }),
  callback = function(args)
    local file = vim.api.nvim_buf_get_name(args.buf)
    local buftype = vim.api.nvim_get_option_value("buftype", { buf = args.buf })

    if not vim.g.ui_entered and args.event == "UIEnter" then
      vim.g.ui_entered = true
    end

    if file ~= "" and buftype ~= "nofile" and vim.g.ui_entered then
      vim.api.nvim_exec_autocmds("User", { pattern = "FilePost", modeline = false })
      vim.api.nvim_del_augroup_by_name "NvFilePost"

      vim.schedule(function()
        vim.api.nvim_exec_autocmds("FileType", {})

        -- if vim.g.editorconfig then
        --   require("editorconfig").config(args.buf)
        -- end
      end)
    end
  end,
})

------------------------------------ highlight color ----------------------------------------:
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
-- some ui settings
vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
    -- Cusor LinNr color
    -- vim.api.nvim_set_hl(0, "NetrwDir", { ctermfg = "Blue", fg = "#698DDA" })

    -- vim.api.nvim_set_hl(0, "ShowBreak", { fg = "White", italic = true, underline = true })
    -- vim.api.nvim_set_hl(0, "LineNr", { fg = "#4b515d" })
    -- vim.api.nvim_set_hl(0, "CursorLinNr", { fg = "#000000" })

    -- optional
    vim.opt.guicursor = "n-v-c:block-Cursor,n-v-c-i:blinkon1,i:ver1000-Cursor,r-cr-o:hor100-cursor"
    -- vim.api.nvim_set_hl(0, "cursor", { background = "#eb6f92", foreground = "white"})

    -- winbar
    -- vim.api.nvim_set_hl(0, "WinBar", { fg = "#000000", bg = "#ffffff", bold = true })
    -- vim.api.nvim_set_hl(0, "WinBarNC", { fg = "#444444", bg = "#000000" }) -- Inactive window
  end,
})

-- TODO:
------------------------------------------- Autocmd for file type --0-00000----------------------------------

--[[ Markdwon ]]
local mdgroup = vim.api.nvim_create_augroup("mdgroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = mdgroup,
  pattern = "*.md",
  callback = function()
    set.shiftwidth = 4
    set.tabstop = 4
    set.softtabstop = 4
    set.textwidth = 150
    vim.opt.foldlevel = 99

    set.wrap = true
    set.spell = true
    set.number = false
    set.relativenumber = false
    -- set.conceallevel = 2
    vim.g.markdown_fenced_languages = { "cpp", "python", "bash=sh", "javascript", "json", "yaml", "vim", "lua" }
    -- vim.keymap.set("n", "<leader>p", "<cmd>PasteImage<cr>", { desc = "Paste the image" })
  end,
})

--[[ -- json ]]
local jsongroup = vim.api.nvim_create_augroup("jsongroup", { clear = true })
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = jsongroup,
  pattern = "*.json",
  callback = function()
    set.conceallevel = 0
    set.shiftwidth = 2
    set.tabstop = 2
    set.softtabstop = 2
    set.textwidth = 150

    vim.bo.filetype = "jsonc"
  end,
})


-- local cppgroup = vim.api.nvim_create_augroup("cppgroup", { clear = true })
-- vim.api.nvim_create_autocmd("FileType", {
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
