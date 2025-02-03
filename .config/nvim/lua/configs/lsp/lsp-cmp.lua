-- NOTE: https://github.com/linkarzu/dotfiles-latest/blob/main/neovim/neobean/lua/plugins/blink-cmp.lua
-- NOTE: check the https://github.com/honza/vim-snippets/tree/master/snippets

require("blink.cmp").setup {
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
    -- ["<S-Tab>"] = { "snippet_backward", "fallback" },

    cmdline = {
      preset = "default",
    },
  },

  appearance = {
    use_nvim_cmp_as_default = false,
    nerd_font_variant = "mono",
  },

  -- snippets = {preset = "luasnip"},

  sources = {
    default = { "lazydev", "lsp", "path", "snippets", "buffer" },
    cmdline = {},
    providers = {
      lazydev = {
        name = "LazyDev",
        enabled = true,
        module = "lazydev.integrations.blink",
        score_offset = 70,
      },
      lsp = {
        name = "lsp",
        enabled = true,
        module = "blink.cmp.sources.lsp",
        score_offset = 100,
      },
      path = {
        name = "Path",
        module = "blink.cmp.sources.path",
        -- opts = {
        --   trailing_slash = false,
        --   label_trailing_slash = true,
        --   get_cwd = function(context)
        --     return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
        --   end,
        --   show_hidden_files_by_default = true,
        -- },
        score_offset = 80,
      },
      -- luasnip = {
      --   name = "luasnip",
      --   enabled = true,
      --   module = "blink.cmp.sources.luasnip",
      --   fallbacks = { "snippets" },
      --   -- min_keyword_length = 4,
      --   score_offset = 80,
      -- },
      buffer = {
        name = "Buffer",
        enabled = true,
        module = "blink.cmp.sources.buffer",
        -- min_keyword_length = 4,
        score_offset = 70,
      },
      snippets = {
        name = "snippets",
        enabled = true,
        module = "blink.cmp.sources.snippets",
        -- min_keyword_length = 4,
        score_offset = 85,
      },

      -- copilot = {
      --   name = "copilot",
      --   enabled = true,
      --   module = "blink-cmp-copilot",
      --   kind = "Copilot",
      --   min_keyword_length = 6,
      --   score_offset = -100, -- the higher the number, the higher the priority
      --   async = true,
      -- },

    },
  },

  completion = {
    menu = {
      draw = {
        treesitter = { "lsp" },
        -- columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
      },
      border = "single",
      auto_show = function(ctx)
        return ctx.mode ~= "cmdline" or not vim.tbl_contains({ "/", "?" }, vim.fn.getcmdtype())
      end,
    },
    trigger = { show_on_keyword = true },
    documentation = { auto_show = true, auto_show_delay_ms = 200, window = { border = "rounded" } },
    -- ghost_text = {
    --   enabled = vim.g.ai_cmp,
    -- },
  },
  signature = { enabled = true },
}

-- set scroll bar appearance
-- vim.api.nvim_set_hl(0, "BlinkCmpScrollBarThumb", { bg = "#000000" })
-- vim.api.nvim_set_hl(0, "BlinkCmpScrollBarGutter", { bg = "#000000" })
