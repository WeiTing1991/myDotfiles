return {
  --https://github.com/folke/trouble.nvim
  {
    "folke/trouble.nvim",
    event = "BufEnter",
    config = function()
      require("trouble").setup({
        icons = true,
      })

      vim.keymap.set("n", "<leader>tt", function()
        require("trouble").toggle()
      end, { desc = "toggle trouble" })

      vim.keymap.set("n", "<leader>tw", function()
        require("trouble").toggle("workspace_diagnostics")
      end, { desc = "workspace_diagnostics" })
      vim.keymap.set("n", "<leader>td", function()
        require("trouble").toggle("document_diagnostics")
      end, { desc = "document_diagnostics" })
      vim.keymap.set("n", "<leader>tq", function()
        require("trouble").toggle("quickfix")
      end, { desc = "quickfix" })
      vim.keymap.set("n", "<leader>tl", function()
        require("trouble").toggle("loclist")
      end, { desc = "loclist" })
      vim.keymap.set("n", "tgR", function()
        require("trouble").toggle("lsp_references")
      end, { desc = "lsp_references" })
    end,
  },
}
