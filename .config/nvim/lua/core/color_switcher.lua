-- theme_switcher.lua

local config = {
  dark_theme = "bella-gruvbox",
  light_theme = "bella-gruvbox",
}

-- Variable to track current theme state
local current_mode = "dark"

-- Function to toggle between themes
local function toggle_theme()
  if current_mode == "dark" then
    -- Switch to light theme
    -- vim.cmd("colorscheme " .. config.light_theme)
    vim.o.background = "light"
    -- vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.light)
    vim.cmd "set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor"
    current_mode = "light"
    print("Switched to light theme: " .. config.light_theme)
  else
    -- Switch to dark theme
    -- vim.cmd("colorscheme " .. config.dark_theme)
    vim.o.background = "dark"
    -- vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.dark)
    vim.cmd "set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor"
    current_mode = "dark"
    print("Switched to dark theme: " .. config.dark_theme)
  end
end

-- Function to set specific theme
local function set_theme(mode)
  if mode == "dark" then
    vim.cmd("colorscheme " .. config.dark_theme)
    vim.o.background = "dark"
    vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.dark)
    vim.cmd "set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor"
    current_mode = "dark"
    print("Set dark theme: " .. config.dark_theme)
  elseif mode == "light" then
    vim.cmd("colorscheme " .. config.light_theme)
    vim.o.background = "light"
    vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.light)
    vim.cmd "set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor"
    current_mode = "light"
    print("Set light theme: " .. config.light_theme)
  end
end

-- Create user commands
vim.api.nvim_create_user_command("ThemeToggle", toggle_theme, {})
vim.api.nvim_create_user_command("ThemeDark", function()
  set_theme "dark"
end, {})
vim.api.nvim_create_user_command("ThemeLight", function()
  set_theme "light"
end, {})


-- Function to apply just cursor color (can be used separately)
local function apply_cursor_color(mode)
  if mode == "dark" then
    vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.dark)
  elseif mode == "light" then
    vim.cmd("highlight Cursor guifg=bg guibg=" .. config.cursor_color.light)
  end
  vim.cmd "set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor"
end

-- Create command to set cursor color only
vim.api.nvim_create_user_command("CursorDark", function()
  apply_cursor_color "dark"
end, {})
vim.api.nvim_create_user_command("CursorLight", function()
  apply_cursor_color "light"
end, {})

return {
  toggle = toggle_theme,
  set_dark = function()
    set_theme "dark"
  end,
  set_light = function()
    set_theme "light"
  end,
  cursor_dark = function()
    apply_cursor_color "dark"
  end,
  cursor_light = function()
    apply_cursor_color "light"
  end,
  config = config,
}
