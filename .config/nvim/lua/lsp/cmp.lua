require("blink.cmp").setup {

  -- NOTE: https://cmp.saghen.dev/configuration/keymap.html
  keymap = {
    preset = "default",

    ["<C-y>"] = { "show", "show_documentation", "hide_documentation" },
    -- ['<C-e>'] = { 'hide' },
    -- ['<C-y>'] = { 'select_and_accept' },

    ["<C-e>"] = { "hide", "fallback" },
    -- ['<Tab>'] = { 'select_and_accept' },

    ["<C-p>"] = { "select_prev", "fallback" },
    ["<C-n>"] = { "select_next", "fallback" },

    ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    ["<C-f>"] = { "scroll_documentation_down", "fallback" },

    ["<Tab>"] = {
      function(cmp)
        if cmp.snippet_active() then
          return cmp.accept()
        else
          return cmp.select_and_accept()
        end
      end,
      "snippet_forward",
      "fallback",
    },
    ["<S-Tab>"] = { "snippet_backward", "fallback" },

    cmdline = {
      preset = "super-tab",
    },
  },

  appearance = {
    use_nvim_cmp_as_default = true,
    nerd_font_variant = "mono",
  },

  sources = {
    default = { "lazydev", "lsp", "path", "snippets", "buffer" },
    -- cmdline = {},
    providers = {
      lazydev = {
        name = "LazyDev",
        enabled = true,
        module = "lazydev.integrations.blink",
        score_offset = 100,
      },

      lsp = {
        name = "lsp",
        enabled = true,
        module = "blink.cmp.sources.lsp",
        score_offset = 1000,
      },
    },
  },
  completion = {
    menu = {
      border = "rounded",
      auto_show = function(ctx)
        return ctx.mode ~= "cmdline" or not vim.tbl_contains({ "/", "?" }, vim.fn.getcmdtype())
      end,
    },
    trigger = { show_on_keyword = true },
    documentation = { window = { border = "rounded" } },
  },
  signature = { enabled = true },
}
-- set scroll bar appearance
vim.api.nvim_set_hl(0, "BlinkCmpScrollBarThumb", { bg = "#000000" })
vim.api.nvim_set_hl(0, "BlinkCmpScrollBarGutter", { bg = "#000000" })
