return {
  --LSP config
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      { "j-hui/fidget.nvim",           opt = {} },
      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      { "folke/neodev.nvim",           opts = {} },
      { "jay-babu/mason-nvim-dap.nvim" },
    },
    config = function()
      require("lsp.lsp-init")    -- lsp engine
      require("lsp.cmp")         -- completion
      require("lsp.lsp-keymaps") -- register

      --only if load with lspconfig and mason
      require('lsp.dap-init')
    end,
  },
  -- LSP Sources && Modules
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer", -- source for text in buffer
      "hrsh7th/cmp-path",   -- source for file system paths
      --"onsails/lspkind.nvim", -- vs-code like pictograms
      "hrsh7th/cmp-nvim-lsp-signature-help",
    },
  },
  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      {
        "L3MON4D3/LuaSnip",
        build = (function()
          -- Build Step is needed for regex support in snippets
          -- This step is not supported in many windows environments
          -- Remove the below condition to re-enable on windows
          if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then return end
          return "make install_jsregexp"
        end)(),
      },
      "saadparwaiz1/cmp_luasnip",
      -- Adds other completion capabilities.
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-cmdline",
      --"hrsh7th/cmp-calc",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "onsails/lspkind-nvim",
    },
  },
  -- LSP saga
  {
    "nvimdev/lspsaga.nvim",
    event = "BufEnter",
    config = function()
      require("lspsaga").setup({
        ui = {
          --enabled = false,
          --background = false,
          code_action = "  ",
        },
      })
    end,
  },
  -- formater and linter
  {
    "nvimtools/none-ls.nvim",
    dependencies = {
      "nvimtools/none-ls-extras.nvim",
    },
    event = "BufEnter",
    config = function()
      require("lsp.none-ls")
    end,
  },
  ------------------------------------------------------------------------------------------------
  -- Java
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    config = function()
      vim.cmd([[
                augroup jdtls_lsp
                autocmd!
                autocmd FileType java lua require'lsp.jdtls.jdtls-setup'.setup()
                augroup end
                ]])
    end,
  },
}
