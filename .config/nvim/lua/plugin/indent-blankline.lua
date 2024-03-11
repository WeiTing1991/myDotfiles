return {
  -- TODO: check the setup
  {
    "lukas-reineke/indent-blankline.nvim",
    enabled = false,
    event = "BufEnter",
    main = "ibl",
    opts = {},
    config = function()
      local highlight = {
        "Grey",
      }
      local hooks = require("ibl.hooks")
      -- create the highlight groups in the highlight setup hook, so they are reset
      -- every time the colorscheme changes
      hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
        vim.api.nvim_set_hl(0, "Grey", { fg = "#595959" })
      end)
      require("ibl").setup({
        debounce = 10,
        indent = {
          char = "╎",
          tab_char = { "╎", "╎" },
          --highlight = { "Function", "Label" },
          smart_indent_cap = true,
          priority = 10,
          repeat_linebreak = true,
          highlight = highlight,
        },
        -- whitespace = { highlight = { "Whitespace", "NonText" } },
      })
    end,
  },
}
