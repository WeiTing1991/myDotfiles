require("conform").setup({
  notify_on_error = false,
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
  formatters_by_ft = require("lsp.formatting"),
})

vim.keymap.set("n", "<leader>m", function()
  require("conform").format({ async = true, lsp_fallback = true })
end, { desc = "formattting" })
