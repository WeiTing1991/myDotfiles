local cmp = require("cmp")
local luasnip = require("luasnip")
local lspkind = require("lspkind")

luasnip.config.setup({})
local luasnip_loaders = require("luasnip.loaders.from_vscode")
luasnip_loaders.lazy_load()

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    completion = { completeopt = "menu,menuone,noinsert" },
    documentation = cmp.config.window.bordered(),
  },

  --Please read `:help ins-completion`, it is really good!
  mapping = cmp.mapping.preset.insert({
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),

    ["<C-l>"] = cmp.mapping(function()
      if luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      end
    end, { "i", "s" }),
    ["<C-h>"] = cmp.mapping(function()
      if luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      end
    end, { "i", "s" }),
  }),

  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" }, -- For luasnip users.
    { name = "path" }, -- file system paths
    { name = "nvim_lsp_signature_help" },
    { name = "nvim_lua" },
    --{ name = "buffer" },
  }),
  formatting = {
    format = lspkind.cmp_format({ with_text = true, maxwidth = 50 }),
  },
  vim.diagnostic.config({
    -- update_in_insert = true,
    float = {
      focusable = false,
      style = "minimal",
      border = "rounded",
      source = "always",
      header = "",
      prefix = "",
    },
  }),
})
