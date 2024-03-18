return {
  {
    "mfussenegger/nvim-dap",
    event = "BufEnter",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "jay-babu/mason-nvim-dap.nvim",
      -- Add own debuggers here
      "leoluz/nvim-dap-go",
    },
  },
}
