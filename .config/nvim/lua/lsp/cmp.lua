local cmp = require "cmp"
local luasnip = require "luasnip"
local lspkind = require "lspkind"

luasnip.config.setup({})
local luasnip_loaders = require("luasnip.loaders.from_vscode")
luasnip_loaders.lazy_load()

cmp.setup {
  performance = {
    max_view_entries = 100,
    -- debounce = 250,
    -- throttle = 2000,
    -- fetching_timeoul = 1400,
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
    end,
  },
  completion = { completeopt = "menuone,noinsert,noselect" },
  window = {
    completion = cmp.config.window.bordered {
      --border = vim.cfg.ui__float_border,
      winhighlight = "CursorLine:PmenuSel,NormalFloat:NormalFloat,FloatBorder:FloatBorder",
      winblend = 0,
    },
    documentation = cmp.config.window.bordered {
      winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
      --border = vim.cfg.ui__float_border,
    },
  },
  --Please read `:help ins-completion`, it is really good!

  mapping = cmp.mapping.preset.insert {
    ["<C-p>"] = cmp.mapping.select_prev_item({behavior = cmp.ConfirmBehavior.Insert, select = true}),
    ["<C-n>"] = cmp.mapping.select_next_item({behavior = cmp.ConfirmBehavior.Insert, select = true}),
    ["<C-e>"] = cmp.mapping.close(),
    ["<C-space>"] = cmp.mapping.complete();
    ["<tab>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Insert, select = true },

    -- ["<C-l>"] = cmp.mapping(function()
    --   if luasnip.expand_or_locally_jumpable() then
    --     luasnip.expand_or_jump()
    --   end
    -- end, { "i", "s" }),
    -- ["<C-h>"] = cmp.mapping(function()
    --   if luasnip.locally_jumpable(-1) then
    --     luasnip.jump(-1)
    --   end
    -- end, { "i", "s" }),
  },
  experimental = {
    native_menu = false,
    ghost_text = false, -- this feature conflict with copilot.vim's preview.
  },
  sources = cmp.config.sources {
    { name = "nvim_lsp", priority = 10, max_item_count = 10 },
    { name = "luasnip", priority = 6, max_item_count = 5 }, -- For luasnip users.

    { name = "nvim_lua", priority = 10, ft = "lua" },
    -- java
    { name = "nvim-jdtls", priority = 10, ft = "java", max_item_count = 8 },

    { name = "nvim_lsp_signature_help", priority = 6, max_item_count = 5 },
    { name = "path", priority = 4 },
    { name = "buffer", priority = 8, keyword_length = 2, max_item_count = 4 },
  },
  formatting = {
    format = lspkind.cmp_format {
      maxwidth = 50
    },
  },
}
