return {
  -- https://github.com/folke/todo-comments.nvim
  {
    'folke/todo-comments.nvim',
    event = 'BufEnter',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {},
  },
  -- comment
  -- https://github.com/numToStr/Comment.nvim
  {
    'numToStr/Comment.nvim',
    event = 'BufEnter',
    opts = {
      -- add any options here
    },
  },
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = true,
    -- use opts = {} for passing setup options
    -- this is equalent to setup({}) function
  },
  --Annotation
  {
    'danymat/neogen',
    event = 'VeryLazy',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    config = function()
      require('neogen').setup({})
      -- ref: https://github.com/danymat/neogen?tab=readme-ov-file#configuration
      --languages = {
      --	["python.google_docstrings"] = require("neogen.configurations.python"),
      --}
    end,
  },
  -- colorizer
  {
    -- https://github.com/NvChad/nvim-colorizer.lua
    'NvChad/nvim-colorizer.lua',
    event = 'VeryLazy',
    config = function()
      require('colorizer').setup({
        user_default_options = {
          names = false, -- "Name" codes like Blue
        },
      })
      vim.keymap.set('n', '<leader>cz', vim.cmd.ColorizerToggle, { desc = 'toggle colorizer' })
    end,
  },
  {
    -- NOTE: Useful website for icon.
    -- https://patorjk.com/software/taag/#p=display&f=Small&t=LE%60T%20WORK
    'goolord/alpha-nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      local alpha = require('alpha')
      local dashboard = require('alpha.themes.startify')
      dashboard.section.header.val = {
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[        _    ___ _ _____  __      _____  ___ _  _        ]],
        [[       | |  | __( )_   _| \ \    / / _ \| _ \ |/ /       ]],
        [[       | |__| _| \| | |    \ \/\/ / (_) |   / ' <        ]],
        [[       |____|___|   |_|     \_/\_/ \___/|_|_\_|\_\       ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
        [[                                                         ]],
      }
      alpha.setup(dashboard.opts)
    end,
  },
  -- Collection of various small independent plugins/modules
  {
    -- "echasnovski/mini.starter",
    -- version = "*",
    -- config = function()
    --   require("mini.starter").setup({
    --     header = "HI LET`S WORK",
    --   })
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

    --Check out: https://github.com/echasnovski/mini.nvim
    -- end,
  },
  --indentscope
  {
    'echasnovski/mini.indentscope',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      draw = {
        delay = 0,
        animation = function() return 0 end,
        priority = 2,
      },
      options = { border = 'top', try_as_border = true },
      symbol = '▏',
    },
    config = function(_, opts)
      require('mini.indentscope').setup(opts)
      -- Disable for certain filetypes
      vim.api.nvim_create_autocmd({ 'FileType' }, {
        desc = 'Disable indentscope for certain filetypes',
        callback = function()
          local ignore_filetypes = {
            'help',
            'lazy',
            'mason',
            'neo-tree',
            'NvimTree',
            'toggleterm',
            'Trouble',
          }
          if vim.tbl_contains(ignore_filetypes, vim.bo.filetype) then vim.b.miniindentscope_disable = true end
        end,
      })
    end,
  },
}
