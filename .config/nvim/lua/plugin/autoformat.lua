return { -- Autoformat
  "stevearc/conform.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    require("conform").setup({
      notify_on_error = false,
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
      formatters_by_ft = require("lsp.formatting"), 

      vim.keymap.set({ "n", "v" }, "<leader>==", function()
        local conform = require("conform")
        conform.format({
          lsp_fallback = true,
          async = false,
          timeout_ms = 500,
        })
      end, { desc = "Format file or range" }),
    })
  end,
}
