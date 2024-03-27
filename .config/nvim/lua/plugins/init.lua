return {
  {
    'EdenEast/nightfox.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('config.colors-nightfox')
      -- vim.cmd([[colorscheme carbonfox]])
    end,
  },
  {
    'catppuccin/nvim',
    --enabled = false,
    lazy = false,
    name = 'catppuccin',
    priority = 1000,
    config = function()
      require('config.colors-catppuccin')
      --vim.cmd([[colorscheme rose-pine]])
    end,
  },
  {
    'rose-pine/neovim',
    enabled = false,
    name = 'rose-pine',
    lazy = false,
    priority = 1000,
    config = function()
      require('config.colors-rose-pine')
      --vim.cmd([[colorscheme rose-pine]])
    end,
  },
  { 'tpope/vim-sleuth', event = 'BufEnter' },
  -- treesitter
  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    event = 'VeryLazy',
    build = ':TSUpdate',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    config = function() require('config.treesitter') end,
  },

  -- telescope
  {
    'nvim-telescope/telescope.nvim',
    event = 'VimEnter',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
      },
      { 'nvim-telescope/telescope-file-browser.nvim' },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      { 'nvim-tree/nvim-web-devicons' },
    },
    config = function() require('config.telescope') end,
  },
  --LSP config
  {
    'neovim/nvim-lspconfig',
    dependencies = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      { 'j-hui/fidget.nvim', opt = {} },
      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      { 'folke/neodev.nvim', opts = {} },
      { 'jay-babu/mason-nvim-dap.nvim' },
    },
    config = function()
      require('lsp.lsp-init') -- lsp engine
      require('lsp.cmp') -- completion
      --only if load with lspconfig and mason
      require('lsp.dap-init')
    end,
  },
  -- LSP Sources && Modules
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer', -- source for text in buffer
      'hrsh7th/cmp-path', -- source for file system paths
      --"onsails/lspkind.nvim", -- vs-code like pictograms
      'hrsh7th/cmp-nvim-lsp-signature-help',
    },
  },
  -- Autocompletion
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      {
        'L3MON4D3/LuaSnip',
        build = (function()
          -- Build Step is needed for regex support in snippets
          -- This step is not supported in many windows environments
          -- Remove the below condition to re-enable on windows
          if vim.fn.has('win32') == 1 or vim.fn.executable('make') == 0 then return end
          return 'make install_jsregexp'
        end)(),
      },
      'saadparwaiz1/cmp_luasnip',
      -- Adds other completion capabilities.
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      --"hrsh7th/cmp-cmdline",
      --"hrsh7th/cmp-calc",
      --"hrsh7th/cmp-nvim-lua",
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'onsails/lspkind-nvim',
    },
  },
  -- LSP saga
  {
    'nvimdev/lspsaga.nvim',
    event = 'BufEnter',
    config = function()
      require('lspsaga').setup({
        ui = {
          --enabled = false,
          background = false,
          code_action = '  ',
        },
      })
    end,
  },
  -- Autoformat
  {
    'stevearc/conform.nvim',
    event = { 'BufWritePre' },
    config = function() require('config.autoformat') end,
  },
  -- show keymaps
  {
    'folke/which-key.nvim',
    enabled = false,
    event = 'VeryLazy', -- Sets the loading event to 'VeryLazy'
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    config = function() -- This is the function that runs, AFTER loading
      require('which-key').setup({})
      require('which-key').register()
    end,
  },
  -- statusline
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    event = 'VimEnter',
    config = function() require('config.lualine') end,
  },
  -- tree
  {
    'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function() require('config.nvim-tree') end,
  },
  -- transpant background
  {
    'xiyaowong/transparent.nvim',
    lazy = false,
    config = function() require('config.transparent') end,
  },
}
