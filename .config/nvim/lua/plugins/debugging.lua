return {
  -- debugger
  {
    'mfussenegger/nvim-dap',
    event = 'BufEnter',
    dependencies = {
      'rcarriga/nvim-dap-ui',
      'theHamsta/nvim-dap-virtual-text',
      'jay-babu/mason-nvim-dap.nvim',
      -- Add own debuggers here
      'leoluz/nvim-dap-go',
    },
  },
  -- test
  {
    'nvim-neotest/neotest',
    event = 'BufEnter',
    dependencies = {
      'nvim-neotest/nvim-nio',
      'nvim-lua/plenary.nvim',
      'antoinemadec/FixCursorHold.nvim',
      'nvim-treesitter/nvim-treesitter',
    },
  },
}
