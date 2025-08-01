require("blink.cmp").setup({
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
        cmp.show({ providers = { "snippets" } })
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
    list = { selection = { preselect = true, auto_insert = true }, max_items = 25 },
    documentation = { auto_show = true, auto_show_delay_ms = 300, window = { border = "single" } },
  },

  cmdline = {
    enabled = true,
    -- use 'inherit' to inherit mappings from top level `keymap` config
    keymap = { preset = "cmdline" },
    sources = function()
      local type = vim.fn.getcmdtype()
      -- Search forward and backward
      if type == "/" or type == "?" then
        return { "buffer" }
      end
      -- Commands
      if type == ":" or type == "@" then
        return { "cmdline" }
      end
      return {}
    end,
    completion = {
      trigger = {
        show_on_blocked_trigger_characters = {},
        show_on_x_blocked_trigger_characters = {},
      },
      list = {
        selection = {
          -- When `true`, will automatically select the first item in the completion list
          preselect = true,
          -- When `true`, inserts the completion item automatically when selecting it
          auto_insert = true,
        },
      },
      -- Whether to automatically show the window when new completion items are available
      menu = { auto_show = false },
      -- Displays a preview of the selected item on the current line
      ghost_text = { enabled = true },
    },
  },

  appearance = {
    nerd_font_variant = "mono",
    -- kind_icons = require("icon").symbol_kinds,
  },

  sources = {
    default = function()
      -- local sources = { "lazydev", "dadbod", "lsp", "path", "snippets", "omni", "buffer" }
      local sources = { "lazydev", "lsp", "path", "snippets", "omni", "buffer" }
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
      dadbod = { name = "Dadbod", module = "vim_dadbod_completion.blink" },
    },
  },
  fuzzy = { implementation = "prefer_rust_with_warning" },
  signature = { enabled = true },
})
