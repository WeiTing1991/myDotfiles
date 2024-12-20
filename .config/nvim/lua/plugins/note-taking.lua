return {

  -- markdwon pretty render
  {
    "MeanderingProgrammer/render-markdown.nvim",
    enabled = false,
    dependencies = { "nvim-treesitter/nvim-treesitter", "echasnovski/mini.icons" },
    opt = {},
    config = function()
      vim.keymap.set("n", "<C-e>", function()
        require('render-markdown').toggle()
      end, { desc = "toggle markdown render" })
    end,
  },

}

