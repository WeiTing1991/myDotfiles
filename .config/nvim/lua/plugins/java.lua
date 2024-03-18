return {
  -- specific languages
  {
    "mfussenegger/nvim-jdtls",
    config = function()
      vim.cmd([[
                augroup jdtls_lsp
                autocmd!
                autocmd FileType java lua require'jdtls.jdtls-setup'.setup()
                augroup end
                ]])
    end,
  },
}
