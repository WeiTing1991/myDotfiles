return {
  {
    "williamboman/mason.nvim",
    dependencies = {
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      "jayp0521/mason-nvim-dap.nvim",
    },
  },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/nvim-cmp",
      { "j-hui/fidget.nvim", opt = {} },
    },
    config = function()
      require("lsp.lsp-init") -- lsp engine
      require("lsp.mason") -- lsp management
      require("lsp.cmp") -- completion
      --require("lsp.dap") -- debuggers management
    end,
  },

  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      config = function()
        require("dap.dap-init")
      end,
    },
  },
  {
    "nvimdev/lspsaga.nvim",
    config = function()
      require("lspsaga").setup({
        ui = {
          --enabled = false,
          code_action = "  ",
        },
      })
    end,
  },
  -- LSP Sources && Modules
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-calc",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "onsails/lspkind-nvim",
    },
  },
  {
    "creativenull/efmls-configs-nvim",
    version = "v1.x.x", -- version is optional, but recommended
    dependencies = { "neovim/nvim-lspconfig" },
    config = function()
      require("lsp.linter")
    end,
  },
  {
    "mfussenegger/nvim-jdtls",
  },
}
