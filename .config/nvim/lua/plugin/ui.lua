return {
  -- https://github.com/echasnovski/mini.animate
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    opts = {},
  },
  {

    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    config = function()
      --	local lazy_status = require("lazy.status") -- to configure lazy pending updates count
      require("lualine").setup({
        options = {
          theme = "dracula",
          component_separators = "|",
          section_separators = { left = "", right = "" },
          refresh = {
            statusline = 500,
            tabline = 500,
            winbar = 500,
          },
        },
        sections = {
          lualine_a = {
            {
              "buffers",
              show_modified_status = false,
              mode = 4,
              buffers_color = {},
              separator = { left = "" },
              right_padding = 2,
            },
          },
          lualine_b = {},
          lualine_c = { "branch", "diff" },
          lualine_x = {},
          lualine_y = {
            "filetype",
            {
              function()
                local msg = "No Active Lsp"
                local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
                local clients = vim.lsp.get_active_clients()
                if next(clients) == nil then
                  return msg
                end
                for _, client in ipairs(clients) do
                  local filetypes = client.config.filetypes
                  if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                    return client.name
                  end
                end
                return msg
              end,
            },
            "progress",
          },
          lualine_z = {
            "mode",
            { "location", separator = { right = "" }, left_padding = 2 },
          },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = { "location" },
        },
        tabline = {},
        extensions = {},
      })
    end,
  },
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    opts = {},
  },
  {
    "prichrd/netrw.nvim",
    enabled = false,
    lazy = false,
    config = function()
      require("netrw").setup({
        -- Put your configuration here, or leave the object empty to take the default
        -- configuration.
        icons = {
          symlink = "", -- Symlink icon (directory and file)
          directory = "", -- Directory icon
          file = "", -- File icon
        },
        use_devicons = true, -- Uses nvim-web-devicons if true, otherwise use the file icon specified above
        mappings = {}, -- Custom key mappings
      })
    end,
  },
}
