return {
  "mbbill/undotree",
  lazy = false,
  config = function()
    vim.keymap.set("n", "<leader><F3>", vim.cmd.UndotreeToggle, { desc = "open undotree" })
  end,
}
