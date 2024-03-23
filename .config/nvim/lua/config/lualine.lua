--	local lazy_status = require("lazy.status") -- to configure lazy pending updates count
require('lualine').setup({
  options = {
    theme = 'dracula',
    component_separators = '|',
    section_separators = { left = '', right = '' },
    refresh = {
      statusline = 500,
      tabline = 500,
      winbar = 500,
    },
  },
  sections = {
    lualine_a = {
      { 'mode', separator = { left = '' }, right_padding = 2 },
      {
        'buffers',
        show_modified_status = false,
        mode = 2,
        --buffers_color = {},
      },
    },
    lualine_b = { 'branch', 'diff' },
    lualine_c = {},
    lualine_x = {},
    lualine_y = {
      {
        function()
          local msg = 'No Active Lsp'
          local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
          local clients = vim.lsp.get_active_clients()
          if next(clients) == nil then return msg end
          for _, client in ipairs(clients) do
            local filetypes = client.config.filetypes
            if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then return client.name end
          end
          return msg
        end,
      },
      'progress',
    },
    lualine_z = {
      { 'location', separator = { right = '' }, left_padding = 2 },
      {
        'diagnostics',
        sources = { 'nvim_diagnostic', 'nvim_lsp' },
        sections = { 'error', 'warn', 'info', 'hint' },
        diagnostics_color = {
          -- Same values as the general color option can be used here.
          error = 'DiagnosticError', -- Changes diagnostics' error color.
          warn = 'DiagnosticWarn', -- Changes diagnostics' warn color.
          info = 'DiagnosticInfo', -- Changes diagnostics' info color.
          hint = 'DiagnosticHint', -- Changes diagnostics' hint color.
        },
        symbols = { error = 'E', warn = 'W', info = 'I', hint = 'H' },
        colored = true, -- Displays diagnostics status in color if set to true.
        update_in_insert = false, -- Update diagnostics in insert mode.
        always_visible = false, -- Show diagnostics even if there are none.
      },
    },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {},
  },
})
