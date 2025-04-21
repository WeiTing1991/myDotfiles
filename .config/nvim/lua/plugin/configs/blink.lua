require("blink.cmp").setup {
  keymap = {
    preset = "default",
    ["<C-y>"] = { "show", "show_documentation", "hide_documentation" },
    -- ["<C-e>"] = { "hide", "fallback" },
    -- ["<Tab>"] = { "select_and_accept" },
    -- ["<C-p>"] = { "select_prev", "fallback" },
    -- ["<C-n>"] = { "select_next", "fallback" },
    -- ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    -- ["<C-f>"] = { "scroll_documentation_down", "fallback" },
    ["<C-space>"] = {
      function(cmp)
        cmp.show { providers = { "snippets" } }
      end,
    },
    --
    ["<Tab>"] = {
      function(cmp)
        if cmp.snippet_active() then
          return cmp.accept()
        else
          return cmp.select_and_accept()
        end
      end,
    },
    --
    -- ["<Backspace>"] = { "snippet_forward", "fallback" },
    -- ["<S-Backspace>"] = { "snippet_backward", "fallback" },
  },
  snippets = { preset = "luasnip" },

  completion = {
    keyword = { range = "full" },
    accept = { auto_brackets = { enabled = false } },
    menu = {
      draw = {
        treesitter = { "lsp" },
      },
      border = "none",
    },
    trigger = { show_on_keyword = true },
    list = { selection = { preselect = true, auto_insert = true }, max_items = 10 },
    documentation = { auto_show = true, auto_show_delay_ms = 300, window = { border = "single" } },
  },

  cmdline = { enabled = false },

  appearance = {
    nerd_font_variant = "mono",
    kind_icons = require("icon").symbol_kinds,
  },

  sources = {
    default = function()
      local sources = { "lazydev", "lsp", "path", "snippets", "buffer" }
      local ok, node = pcall(vim.treesitter.get_node)

      if ok and node then
        if not vim.tbl_contains({ "comment", "line_comment", "block_comment" }, node:type()) then
          table.insert(sources, "path")
        end
        if node:type() ~= "string" then
          table.insert(sources, "snippets")
        end
      end
      return sources
    end,
    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        -- make lazydev completions top priority (see `:h blink.cmp`)
        score_offset = 100,
      },
    },
  },
  fuzzy = { implementation = "prefer_rust_with_warning" },
  signature = { enabled = true },
}
