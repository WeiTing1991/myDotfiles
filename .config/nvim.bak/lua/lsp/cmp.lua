local cmp = require "cmp"
local luasnip = require "luasnip"
local lspkind = require "lspkind"

luasnip.config.setup({})
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup {
  performance = {
    -- max_view_entries = 100,
    -- debounce = 250,
    -- throttle = 2000,
    -- fetching_timeoul = 1400,
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
    end,
  },

  completion = { completeopt = "menu, menuone, noinsert" },
  -- window = {
  --   completion = cmp.config.window.bordered {
  --     --border = vim.cfg.ui__float_border,
  --     winhighlight = "CursorLine:PmenuSel,NormalFloat:NormalFloat,FloatBorder:FloatBorder",
  --     winblend = 0,
  --   },
  --   documentation = cmp.config.window.bordered {
  --     winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
  --     --border = vim.cfg.ui__float_border,
  --   },
  -- },
  -- read `:help ins-completion`
  --
  sorting = {
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.compare.recently_used,
      require("clangd_extensions.cmp_scores"),
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },

  mapping = cmp.mapping.preset.insert {
    ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
    ["<C-e>"] = cmp.mapping.close(),
    ["<C-space>"] = cmp.mapping.complete(),
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
  sources = {
    {
      name = 'lazydev',
      -- set group index to 0 to skip loading LuaLS completions as lazydev recommends it
      group_index = 0,
    },
    { name = "nvim_lsp", priority = 8, max_item_count = 10 },
    { name = "luasnip",  priority = 10, max_item_count = 10 }, -- For luasnip users.

    { name = "path",     priority = 4 },
    { name = "buffer",   priority = 6, keyword_length = 2, max_item_count = 4 },
  },
  formatting = {
    format = lspkind.cmp_format {
      mode = "text",
      menu = {
        nvim_lsp = "[LSP]",
        ultisnips = "[US]",
        path = "[Path]",
        buffer = "[Buffer]",
        emoji = "[Emoji]",
        omni = "[Omni]",
      },
      show_labelDetails = true,
      maxwidth = 40,
      ellipsis_char = "...",
    },
  },
}