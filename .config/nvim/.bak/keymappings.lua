----- Pluglins keymaps -----
local map = vim.keymap.set
local fzf = require("fzf-lua")
local color_swithcer = require("core.color_switcher")
local snacks = require("snacks")
local mini_select = require("core.ui_select")

map("n", "<leader>rr", fzf.registers, { desc = "show register" })
map("n", "<leader>rm", fzf.marks, { desc = "show marks" })

-- lsp enhance
map("n", "<leader>q", function()
  require("goto-preview").close_all_win()
end, { desc = "Close all preview windows" })

-- [[ Override default keymaps ]]
map("n", "<C-q>", function()
  snacks.bufdelete()
end, { desc = "Close current buffer and window" })

--[[ color_swithcer ]]
vim.keymap.set("n", "<leader>tt", function()
  color_swithcer.toggle()
end, { desc = "Toggle cursor color" })

--[[ file tree ]]
map("n", "<leader>d", function()
  require("oil").open()
end, { desc = "Toggle file explorer" })


map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "File tree" })

map("n", "<C-e>", function()
  local bufname = vim.api.nvim_buf_get_name(0)
  local path = vim.fn.fnamemodify(bufname, ":p")

  -- Noop if the buffer isn't valid.
  if path and vim.uv.fs_stat(path) then
    require("mini.files").open(bufname, false)
  end
end, { desc = "Open file in mini.files" })

--[[ fzf ]]
map("n", "<C-f>", fzf.files, { desc = "Find files" })
map("n", "<leader>ff", fzf.files, { desc = "Find files" })
map("n", "<leader>fo", fzf.oldfiles, { desc = "Open recent file" })
map("n", "<leader>fb", fzf.buffers, { desc = "Finde file in opened buffer" })
map("n", "<leader>fj", function()
  require("fzf-lua").files({ cwd = vim.fn.expand("%:p:h") })
end, { desc = "Finde file in current child dir" })
map("n", "<leader>fg", function()
  fzf.live_grep_glob({ search = vim.fn.input("Grep > ") })
end, { desc = "Grep search" })
map("n", "<leader>fk", fzf.keymaps, { desc = "Search keymaps" })
map("n", "<leader>fl", fzf.live_grep, { desc = "Find live grep" })
map("n", "<leader>ft", fzf.tabs, { desc = "Find tabs" })
map("n", "<leader>ftb", fzf.tmux_buffers, { desc = "Find tmux buffer" })

map("n", "z=", fzf.spell_suggest, { desc = "Spelling suggestions" })


--[[ git ]]
-- https://www.naseraleisa.com/posts/diff
map("n", "<leader>gd", ":DiffviewOpen<cr>", { desc = "Git diff view" })
map("n", "<leader>gf", ":DiffviewFileHistory<cr>", { desc = "Git diff view" })
map("n", "<leader>gq", ":DiffClose<cr>", { desc = "Close GitDiff" })

-- inbuffer
map("n", "<leader>gl", function()
  require("core.float_term").float_term(
    "lazygit",
    { size = { width = 0.9, height = 0.85 }, cwd = vim.b.gitsigns_status_dict.root }
  )
end, { desc = "Lazygit" })

-- hunk
map({ "n", "v" }, "<leader>gH", function()
  require("gitsigns").stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
end, { desc = "Stage hunk" })
map("n", "<leader>ghs", ":Gitsign stage_buffer<CR>", { desc = "stage hunk" })
map("n", "<leader>ghh", ":Gitsign preview_hunk<CR>", { desc = "Preview hunk" })
map({ "n", "v" }, "<leader>ghr", function()
  require("gitsigns").reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
end, { desc = "git reset hunk" })
-- map("n", "<leader>gss", ":Gitsign stage_hunk<CR>", { desc = "stage hunk" })
-- map("n", "<leader>ghr", ":Gitsign reset_hunk<CR>", { desc = "reset hunk" })

map("n", "<leader>gs", ":FzfLua git_status<CR>", { desc = "Status" })
map("n", "<leader>gb", ":FzfLua git_branches<CR>", { desc = "Branch" })
map("n", "<leader>gcb", ":FzfLua git_bcommits<CR>", { desc = "Buffer Commits" })
map("n", "<leader>gcc", ":FzfLua git_commits<CR>", { desc = "Commits" })
map("n", "<leader>gb", function()
  snacks.gitbrowse()
end, { desc = "open current github" })

local function commit_current_file()
  local file = vim.fn.expand("%")
  local message = vim.fn.input("Commit message: ")
  vim.cmd("Git add " .. file)
  vim.cmd('Git commit -m "' .. message .. '"')
end
map("n", "<leader>gca", commit_current_file, { desc = "Git commit current file" })

-- map("n", "<leader>gi", ":Gitsign toggle_current_line_blame<CR>", { desc = "currentt line blame" })

-- neogit !!!!
-- https://github.com/NeogitOrg/neogit
local autocmd = vim.api.nvim_create_autocmd
autocmd("BufWinEnter", {
  group = vim.api.nvim_create_augroup("wt/fugitive", {}),
  pattern = "*",
  callback = function()
    if vim.bo.ft ~= "fugitive" then
      return
    end
    local bufnr = vim.api.nvim_get_current_buf()
    local opts = { buffer = bufnr, remap = false }
    vim.keymap.set("n", "<leader>p", function()
      vim.cmd.Git("push")
    end, opts)
    -- rebase always
    vim.keymap.set("n", "<leader>P", function()
      vim.cmd.Git({ "pull", "--rebase" })
    end, opts)
    vim.keymap.set("n", "<leader>t", ":Git push -u origin ", opts)
  end,
})
-- map("n", "gu", "<cmd>diffget //2<CR>", { desc = "Diff get" })
-- map("n", "gh", "<cmd>diffget //3<CR>", { desc = "Diff get" })

--[[ toggle ]]
map("n", "<leader>tc", function()
  require("copilot.suggestion").toggle_auto_trigger()
  if not vim.b.copilot_suggestion_auto_trigger then
    print("Copilot is disabled")
  else
    print("Copilot is enabled")
  end
end, { desc = "Copilot" })

map("n", "<leader>tu", vim.cmd.UndotreeToggle, { desc = "Undotree" })
map("n", "<leader>ta", function()
  require("neogen").generate()
end, { desc = "Annotation" })

--[[ diagnostics ]]
map("n", "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", { desc = "Diagnostics " })
map("n", "<leader>xw", "<cmd>Trouble diagnostics toggle<cr>", { desc = "Diagnostics workspace" })
map("n", "<leader>xq", "<cmd>Trouble qflist toggle <cr>", { desc = "Quickfix List " })
map("n", "<leader>xl", "<cmd>Trouble locflist toggle <cr>", { desc = "Location List " })
