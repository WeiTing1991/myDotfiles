return {
  {
    "saghen/blink.cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    version = "1.*",
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        version = "2.*",
        build = (function()
          if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
            return
          end
          return "make install_jsregexp"
        end)(),
        config = function()
          require("luasnip").setup({
            history = true,
            delete_check_events = "TextChanged",
          })
          local snippet_path = vim.fn.stdpath("config") .. "/snippets"
          require("luasnip.loaders.from_vscode").lazy_load({ paths = { snippet_path } })
        end,
      },
    },
    opts = {
      keymap = {
        preset = "default",
        ["<A-j>"] = { "select_next", "fallback" },
        ["<A-k>"] = { "select_prev", "fallback" },
        ["<C-y>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-space>"] = {
          function(cmp)
            cmp.show({ providers = { "snippets" } })
          end,
        },
        ["<Tab>"] = { "select_and_accept", "fallback" },
      },
      snippets = { preset = "luasnip" },
      completion = {
        keyword = { range = "full" },
        accept = { auto_brackets = { enabled = false } },
        menu = {
          draw = { treesitter = { "lsp" } },
          border = "none",
        },
        trigger = { show_on_keyword = true },
        list = { selection = { preselect = true, auto_insert = true }, max_items = 25 },
        documentation = { auto_show = true, auto_show_delay_ms = 300, window = { border = "single" } },
      },
      cmdline = {
        enabled = true,
        keymap = {
          preset = "cmdline",
          ["<Tab>"] = { "select_and_accept", "fallback" },
          ["<A-j>"] = { "select_next", "fallback" },
          ["<A-k>"] = { "select_prev", "fallback" },
        },
        sources = function()
          local type = vim.fn.getcmdtype()
          if type == "/" or type == "?" then return { "buffer" } end
          if type == ":" or type == "@" then return { "cmdline" } end
          return {}
        end,
        completion = {
          trigger = {
            show_on_blocked_trigger_characters = {},
            show_on_x_blocked_trigger_characters = {},
          },
          list = { selection = { preselect = true, auto_insert = true } },
          menu = {
            auto_show = true,
            draw = {
              columns = {
                { "label", "label_description", gap = 1 },
                { "kind_icon", "kind", gap = 1 },
                { "source_name" },
              },
            },
          },
          ghost_text = { enabled = true },
        },
      },
      appearance = { nerd_font_variant = "mono" },
      sources = {
        default = { "lazydev", "lsp", "path", "snippets", "buffer" },
        providers = {
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            score_offset = 100,
          },
        },
      },
      fuzzy = { implementation = "prefer_rust_with_warning" },
      signature = { enabled = true },
    },
  },
}
