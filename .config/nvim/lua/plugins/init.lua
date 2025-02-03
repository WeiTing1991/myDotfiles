return {
  ---------------------------------------- Base UI ----------------------------------------------------
 { "nvim-lua/plenary.nvim" },
 { "nvim-tree/nvim-web-devicons", lazy = true },
 {
   "nvchad/ui",
    config = function()
      require "nvchad"
    end
 },
 {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
 },
 --theme switcher
 {"nvchad/volt"},

  --------------------------------------- CORE  -------------------------------------------------------
  -- fzf/telescope
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    cmd = "Telescope",
    event = "VimEnter",
    branch = "0.1.x",
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function()
          return vim.fn.executable "make" == 1
        end,
      },
      { "nvim-telescope/telescope-frecency.nvim", version = "*" },
      { "nvim-telescope/telescope-ui-select.nvim" },
    },
    config = function()
      require "configs.telescope"
    end,
  },
}
