return {
  { -- Collection of various small independent plugins/modules
    "echasnovski/mini.nvim",
    event = "BufEnter",
    enabled = true,
    config = function()
      -- https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-indentscope.md
      require("mini.starter").setup({
        header = "Hi LET`S WORK",
      })
      -- Better Around/Inside textobjects
      --
      -- Examples:
      --  - va)  - [V]isually select [A]round [)]paren
      --  - yinq - [Y]ank [I]nside [N]ext [']quote
      --  - ci'  - [C]hange [I]nside [']quote
      --require("mini.ai").setup({ n_lines = 500 })

      -- Add/delete/replace surroundings (brackets, quotes, etc.)
      --
      -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
      -- - sd'   - [S]urround [D]elete [']quotes
      -- - sr)'  - [S]urround [R]eplace [)] [']
      --require('mini.surround').setup()

      --  Check out: https://github.com/echasnovski/mini.nvim
    end,
  },
  {
    "echasnovski/mini.indentscope",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      draw = {
        delay = 0,
        animation = function()
          return 0
        end,
        priority = 2,
      },
      options = { border = "top", try_as_border = true },
      symbol = "▏",
    },
    config = function(_, opts)
      require("mini.indentscope").setup(opts)

      -- Disable for certain filetypes
      vim.api.nvim_create_autocmd({ "FileType" }, {
        desc = "Disable indentscope for certain filetypes",
        callback = function()
          local ignore_filetypes = {
            "help",
            "lazy",
            "leetcode.nvim",
            "mason",
            "neo-tree",
            "NvimTree",
            "toggleterm",
            "Trouble",
          }
          if vim.tbl_contains(ignore_filetypes, vim.bo.filetype) then
            vim.b.miniindentscope_disable = true
          end
        end,
      })
    end,
  },
}
