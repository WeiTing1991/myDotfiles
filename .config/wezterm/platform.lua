local wezterm = require("wezterm")

local triple = wezterm.target_triple
local home = wezterm.home_dir

local M = {
  -- Match on the OS part of the triple, not the whole thing: "darwin" also
  -- covers Intel Macs (x86_64-apple-darwin), which an exact match missed.
  is_windows = triple:find("windows") ~= nil,
  is_macos = triple:find("darwin") ~= nil,
}

M.is_linux = not M.is_windows and not M.is_macos

-- Cross-platform baseline. Every branch below overrides, never removes, so no
-- field can end up nil on a platform we haven't special-cased.
local opts = {
  font_size = 14.0,
  -- Benchmarked both on an M3 Max (200k lines of coloured output, 5 alternating
  -- rounds each): OpenGL 0.92s CPU vs WebGpu 0.94s. No speed reason to prefer
  -- WebGpu, and it costs an sRGB colour shift, thinner glyphs, and a broken
  -- window_background_opacity on Windows/DX12 (wezterm#5790, still open).
  -- OpenGL is also wezterm's own default.
  front_end = "OpenGL",
  -- Only consulted when front_end = "WebGpu"; kept so switching back is one edit.
  webgpu_power_preference = "HighPerformance",
  max_fps = 120,
  freetype_load_target = "Normal",
  color_scheme = "Suannhai Jiufen",
  color_scheme_dirs = { home .. "/project/theme/suannhai-theme/suannhai-wezterm/colors" },
  nvim_bin = "nvim",
  font = wezterm.font_with_fallback({
    {
      family = "Cascadia Code NF",
      harfbuzz_features = { "calt=0" },
    },
    {
      family = "Hack Nerd Font",
    },
    {
      family = "JetBrainsMono Nerd Font",
      harfbuzz_features = { "calt=0" },
    },
  }),
  -- default_prog and window_frame are deliberately absent here: leaving them
  -- unset makes wezterm use $SHELL and its own frame font, which is the right
  -- default on any platform we don't name explicitly.
}

if M.is_windows then
  opts.default_prog = { "C:/Program Files/PowerShell/7/pwsh.exe" }
  opts.font_size = 12.0
  opts.freetype_load_target = "HorizontalLcd"
  opts.color_scheme_dirs = { home .. "/theme/suannhai-theme/suannhai-wezterm/colors" }
  opts.font = wezterm.font_with_fallback({
    {
      family = "CaskaydiaCove Nerd Font Mono",
      harfbuzz_features = { "calt=0" },
    },
    {
      family = "Hack Nerd Font",
    },
    {
      family = "JetBrainsMono Nerd Font",
      harfbuzz_features = { "calt=0" },
    },
  })
  opts.window_frame = {
    font = wezterm.font("Consolas", { weight = "Regular" }),
    font_size = 10.0,
  }
elseif M.is_macos then
  opts.default_prog = { "/bin/zsh", "-l" }
  opts.font_size = 16.0
  opts.freetype_load_target = "Light"
  opts.nvim_bin = "/opt/homebrew/bin/nvim"
  opts.window_frame = {
    font = wezterm.font("SF Pro Text", { weight = "Regular" }),
    font_size = 14.0,
  }
end

M.opts = opts

return M
