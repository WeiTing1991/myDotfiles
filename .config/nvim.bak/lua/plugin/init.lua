return {
  --
  {
    "nvchad/base46",
    -- lazy = false,
    build = function()
      require("base46").load_all_highlights()
    end,
  },
  {
    "nvchad/ui",
    lazy = false,
    config = function()
      require "nvchad"
    end
  },
  {
    "WeiTing1991/staline.nvim",
    lazy = false,
    config = function()
      require "configs.staline"
    end,
  },

  -- {
  --   "nvim-telescope/telescope.nvim",
  --   lazy = true,
  --   cmd = "Telescope",
  --   event = "VimEnter",
  --   branch = "0.1.x",
  --   dependencies = {
  --     {
  --       "nvim-telescope/telescope-fzf-native.nvim",
  --       build = "make",
  --       cond = function()
  --         return vim.fn.executable "make" == 1
  --       end,
  --     },
  --     { "nvim-telescope/telescope-frecency.nvim", version = "*" },
  --     { "nvim-telescope/telescope-ui-select.nvim" },
  --   },
  --   config = function()
  --     require "configs.telescope"
  --   end,
  -- },


function Filename_only_tabline()
  local s = ''
  for i = 1, vim.fn.tabpagenr('$') do
    -- Select the highlighting
    if i == vim.fn.tabpagenr() then
      s = s .. '%#TabLineSel#'
    else
      s = s .. '%#TabLine#'
    end

    -- Set the tab page number (for mouse clicks)
    s = s .. '%' .. i .. 'T'

    -- Get the window number
    local winnr = vim.fn.tabpagewinnr(i)

    -- Get the filename for each tab's active window
    local buflist = vim.fn.tabpagebuflist(i)
    local filename = vim.fn.fnamemodify(vim.fn.bufname(buflist[winnr]), ':t')

    if filename == '' then
      filename = '[No Name]'
    end

    s = s .. ' ' .. filename .. ' '
  end

  -- Fill the rest of the tabline
  s = s .. '%#TabLineFill#'

  return s
end

vim.opt.tabline = "%{%v:lua.Filename_only_tabline()%}"
}
