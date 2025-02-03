-- NOTE: https://github.com/tamton-aquib/staline.nvim

vim.opt.laststatus = 3

-- spell statues
local function spell_check()
  if vim.wo.spell then
    return "  " .. vim.opt.spelllang:get()[1] .. ""
  else
    return "  "
  end
end

-- copilot statues
local function copilot_status()
  if vim.b.copilot_suggestion_auto_trigger then
    return "   "
  else
    return "   "
  end
end
-- indent statues
local function indent_style()
  local style = vim.opt.expandtab:get() and "" or "⇥"
  local tab_width = vim.opt.shiftwidth:get()
  return string.format("%s (%d)", style, tab_width)
end

-- NOTE: try to make a pull request

-- lsp status
local function lsp_formater()
  local buf_clients = vim.lsp.buf_get_clients()
  local server_names = {}
  local has_null_ls = false
  local ignore_lsp_servers = {
    ["null-ls"] = true,
    ["copilot"] = true,
  }

  for _, client in pairs(buf_clients) do
    local client_name = client.name
    if not ignore_lsp_servers[client_name] then
      server_names[#server_names + 1] = client_name
    end
  end

  if package.loaded["null-ls"] then
    local null_ls = nil
    has_null_ls, null_ls = pcall(require, "null-ls")

    if has_null_ls then
      local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
      local null_ls_methods = {
        null_ls.methods.DIAGNOSTICS,
        null_ls.methods.DIAGNOSTICS_ON_OPEN,
        null_ls.methods.DIAGNOSTICS_ON_SAVE,
        null_ls.methods.FORMATTING,
      }

      local get_null_ls_sources = function(methods, name_only)
        local sources = require "null-ls.sources"
        local available_sources = sources.get_available(buf_ft)

        methods = type(methods) == "table" and methods or { methods }

        -- methods = nil or {}
        if #methods == 0 then
          if name_only then
            return vim.tbl_map(function(source)
              return source.name
            end, available_sources)
          end
          return available_sources
        end

        local source_results = {}

        for _, source in ipairs(available_sources) do
          for _, method in ipairs(methods) do
            if source.methods[method] then
              if name_only then
                source_results[#source_results + 1] = source.name
              else
                source_results[#source_results + 1] = source
              end
              break
            end
          end
        end

        return source_results
      end

      local null_ls_builtins = get_null_ls_sources(null_ls_methods, true)
      vim.list_extend(server_names, null_ls_builtins)
    end
  end

  -- if package.loaded["conform"] then
  --   local has_conform, conform = pcall(require, "conform")
  --   if has_conform then
  --     vim.list_extend(
  --       server_names,
  --       vim.tbl_map(function(formatter)
  --         return formatter.name
  --       end, conform.list_formatters(0))
  --     )
  --     if has_null_ls then
  --       server_names = vim.fn.uniq(server_names)
  --     end
  --   end
  -- end

  return #server_names > 0 and table.concat(server_names, ". ") or "NO LSP, FORMATTER  "
  -- condition = function() return vim.o.columns > 70 end,
end

require("staline").setup {
  defaults = {
    expand_null_ls = false, -- This expands out all the null-ls sources to be shown
    -- left_separator = "",
    -- right_separator = "",
    left_separator = "",
    right_separator = "",

    full_path = false,
    branch_symbol = " ",
    bg = "#1e1e2e",
    -- bg = "#000000",
    -- fg = "#000000", -- Foreground text color.
    -- inactive_color = "#303030",
    inactive_bgcolor = "#1e1e2e",
    true_colors = true,   -- true lsp colors.
    font_active = "bold", -- "bold", "italic", "bold,italic", etc
    line_column = " [%l/%L] :%c",
  },

  mode_colors = {},
  mode_icons = {},
  sections = {
    left = { "- ", "-mode", "left_sep_double", "file_name", "branch" },
    mid = { lsp_formater },
    right = { "lsp", spell_check, copilot_status, indent_style, "right_sep_double", "-cwd" },
  },

  inactive_sections = {
    -- left = { "branch" },
    -- mid = { spell_check },
    -- right = {},
  },

  special_table = {
    lazy = { "LAZY", "  " },
    mason = { "MASON", "  " },
    nvdash = { "HOME", "" },
  },

  lsp_symbols = {
    Error = " ",
    Warn = " ",
    Info = " ",
    Hint = "󰌵 ",
  },
}
