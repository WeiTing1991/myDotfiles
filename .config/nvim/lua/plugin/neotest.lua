return {
  {
    -- REF https://github.com/nvim-neotest/neotest?tab=readme-ov-file
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "rcasia/neotest-java",
      config = function()
        require("neotest").setup({
          adapters = {
            require("neotest-java")({
              ignore_wrapper = false, -- whether to ignore maven/gradle wrapper
            }),
          },
          require("neotest-plenary"),
          require("neotest-vim-test")({
            ignore_file_types = { "python", "vim", "lua", "java" },
          }),
        })
        vim.kemaps.set("n", "<leader>to", function()
          require("neotest").run.run(vim.fn.expand("%"))
        end)
        --     { "<leader>tT", function() require("neotest").run.run(vim.loop.cwd()) end, desc = "Run All Test Files" }
        -- ,
        --     { "<leader>tS", function() require("neotest").run.stop() end, desc = "Stop" },
      end,
    },
  },
}
