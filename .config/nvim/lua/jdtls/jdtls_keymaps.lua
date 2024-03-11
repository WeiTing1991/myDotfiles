-- NOTE: Java specific keymaps with which key
vim.cmd(
  "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
)
vim.cmd(
  "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
)
vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
vim.cmd("command! -buffer JdtJol lua require('jdtls').jol()")
vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")
vim.cmd("command! -buffer JdtJshell lua require('jdtls').jshell()")

-- Java specific
vim.keymap.set("n", "<leader>Ji", "<Cmd>lua require'jdtls'.organize_imports()<CR>", { desc = "organize_imports" })
vim.keymap.set("n", "<leader>Jc", "<Cmd>lua require'jdtls'.test_class()<CR>", { desc = "test_class" })
vim.keymap.set("n", "<leader>Jn", "<Cmd>lua require'jdtls'.test_nearest_method()<CR>", { desc = "test_nearest_method" })
vim.keymap.set(
  "v",
  "<leader>Jet",
  "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>",
  { desc = "extract_variable" }
)
vim.keymap.set("n", "<leader>Je", "<Cmd>lua require('jdtls').extract_variable()<CR>", { desc = "extract_variable" })
vim.keymap.set(
  "v",
  "<leader>Jm",
  "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>",
  { desc = "extract_variablej" }
)
vim.keymap.set("n", "<leader>m", "<cmd>lua vim.lsp.buf.formatting()<CR>", { desc = "formatting" })
