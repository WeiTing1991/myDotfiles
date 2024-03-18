return {
  {
    "David-Kunz/gen.nvim",
    event = "BufEnter",
    config = function()
      require("gen").setup({
        display_mode = "split", -- The display mode. Can be "float" or "split".
        show_prompt = true, -- Shows the Prompt submitted to Ollama.
        show_model = true, -- Displays which model you are using at the beginning of your chat session.
        no_auto_close = false, -- Never closes the window automatically.
      })
      vim.keymap.set({ "n", "v" }, "<leader><F9>", ":Gen<CR>", { desc = "ollama task" })
      vim.keymap.set({ "n", "v" }, "<leader><F10>", ":Gen Chat<CR>", { desc = "ollama chat" })
    end,
  },

  {
    -- :help copilot
    "github/copilot.vim",
    event = "BufEnter",
  },
}
