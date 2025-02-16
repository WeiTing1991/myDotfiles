-- NOTE: https://github.com/linkarzu/dotfiles-latest/blob/main/neovim/neobean/lua/plugins/blink-cmp.lua
-- NOTE: check the https://github.com/honza/vim-snippets/tree/master/snippets


-- require("base46").load_all_highlights()

require("blink.cmp").setup {
  keymap = {
    preset = "default",

    ["<C-y>"] = { "show", "show_documentation", "hide_documentation" },
    ["<C-e>"] = { "hide", "fallback" },
    -- ['<Tab>'] = { 'select_and_accept' },

    ["<C-p>"] = { "select_prev", "fallback" },
    ["<C-n>"] = { "select_next", "fallback" },

    ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    ["<C-f>"] = { "scroll_documentation_down", "fallback" },
    -- ['<C-space>'] = { function(cmp) cmp.show({ providers = { 'snippets' } }) end },

    ["<Tab>"] = {
      function(cmp)
        if cmp.snippet_active() then
          return cmp.accept()
        else
          return cmp.select_and_accept()
        end
      end,
    },

    ["<Backspace>"] = { "snippet_forward", "fallback" },
    ["<S-Backspace>"] = { "snippet_backward", "fallback" },

  },

  appearance = {
    use_nvim_cmp_as_default = false,
    nerd_font_variant = "mono",
  },

  enabled = function()
    return not vim.tbl_contains({ "" }, vim.bo.filetype)
        and vim.bo.buftype ~= "prompt"
        and vim.b.completion ~= false
  end,

  completion = {
    keyword = { range = 'full' },
    accept = { auto_brackets = { enabled = false }, },
    menu = {
      draw = {
        treesitter = { "lsp" },
      },
      border = "single",
      auto_show = function(ctx)
        return ctx.mode ~= "cmdline" or not vim.tbl_contains({ "/", "?" }, vim.fn.getcmdtype())
      end,
    },
    list = { selection = { preselect = true, auto_insert = true } },
    trigger = { show_on_keyword = true },
    documentation = { auto_show = true, auto_show_delay_ms = 300, window = { border = "single" } },
    -- ghost_text = {
    --   enabled = vim.g.ai_cmp,
    -- },
  },
  signature = { enabled = true },

  -- snippets
  -- snippets = { preset = 'default' },
  snippets = { preset = 'luasnip' },

  sources = {
    default = { "lsp", "path", "snippets", "buffer" },

  }

  -- providers = {
  --   lsp = {
  --     name = "lsp",
  --     enabled = true,
  --     module = "blink.cmp.sources.lsp",
  --     score_offset = 100,
  --   },
  --   path = {
  --     name = "Path",
  --     module = "blink.cmp.sources.path",
  --     -- opts = {
  --     --   trailing_slash = false,
  --     --   label_trailing_slash = true,
  --     --   get_cwd = function(context)
  --     --     return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
  --     --   end,
  --     --   show_hidden_files_by_default = true,
  --     -- },
  --     -- score_offset = 80,
  --   },
  --   -- luasnip = {
  --   --   name = "luasnip",
  --   --   enabled = true,
  --   --   module = "blink.cmp.sources.luasnip",
  --   --   fallbacks = { "snippets" },
  --   --   -- min_keyword_length = 4,
  --   --   score_offset = 80,
  --   -- },
  --   buffer = {
  --     name = "Buffer",
  --     enabled = true,
  --     module = "blink.cmp.sources.buffer",
  --     -- min_keyword_length = 4,
  --     -- score_offset = 70,
  --   },
  --   snippets = {
  --     name = "snippets",
  --     enabled = true,
  --     module = "blink.cmp.sources.snippets",
  --     -- min_keyword_length = 4,
  --     -- score_offset = 85,
  --   },

  --   -- copilot = {
  --   --   name = "copilot",
  --   --   enabled = true,
  --   --   module = "blink-cmp-copilot",
  --   --   kind = "Copilot",
  --   --   min_keyword_length = 6,
  --   --   score_offset = -100, -- the higher the number, the higher the priority
  --   --   async = true,
  --   -- },
  -- },
}

-- set scroll bar appearance
-- vim.api.nvim_set_hl(0, "BlinkCmpScrollBarThumb", { bg = "#000000" })
-- vim.api.nvim_set_hl(0, "BlinkCmpScrollBarGutter", { bg = "#000000" })
