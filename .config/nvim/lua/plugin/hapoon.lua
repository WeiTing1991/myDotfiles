return {
  "ThePrimeagen/harpoon",
  lazy = false,
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require("harpoon")

    -- REQUIRED
    harpoon:setup()
    -- REQUIRED

    vim.keymap.set("n", "<leader>a", function()
      harpoon:list():append()
    end, { desc = "harpoon append" })
    vim.keymap.set("n", "<leader>aa", function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, { desc = "harpoon list" })

    vim.keymap.set("n", "<C-e-1>", function()
      harpoon:list():select(1)
    end)
    vim.keymap.set("n", "<C-e-2>", function()
      harpoon:list():select(2)
    end)
    vim.keymap.set("n", "<C-e-3>", function()
      harpoon:list():select(3)
    end)
    vim.keymap.set("n", "<C-e-4>", function()
      harpoon:list():select(4)
    end)

    -- Toggle previous & next buffers stored within Harpoon list
    vim.keymap.set("n", "<C-S-P>", function()
      harpoon:list():prev()
    end, { desc = "Go to pervious mark" })
    vim.keymap.set("n", "<C-S-N>", function()
      harpoon:list():next()
    end, { desc = "Go to next mark" })
  end,
}
