return {
  -- tmux navigator
  {
    "christoomey/vim-tmux-navigator",
    lazy = true,
    event = "BufEnter",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },

  -- tree
  {
    "echasnovski/mini.files",
    lazy = false,
    opts = {},
  },
  {
    "mbbill/undotree",
    lazy = true,
    event = "VeryLazy",
  },
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("plugin.configs.nvimtree")
    end,
  },

  -- Markdown
  {
    "iamcco/markdown-preview.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },

  -- diagnostics
  {
    "folke/trouble.nvim",
    lazy = true,
    event = "VeryLazy",
    cmd = "Trouble",
    opts = {
      focus = true,
      auto_preview = true, -- Disable auto-preview
      preview = {
        type = "float",
        relative = "editor",
        size = { width = 0.8, height = 0.3 }, -- Smaller preview
        position = { 0.5, 0.8 },
        border = "single",
      },
    },
  },

  -- column
  {
    "luukvbaal/statuscol.nvim",
    lazy = true,
    event = "BufEnter",
    config = function()
      local builtin = require("statuscol.builtin")
      require("statuscol").setup({
        setopt = true,
        ft_ignore = { "snacks_dashboard", "help", "lazy", "mason", "NvimTree", "undotree" },
        segments = {
          { text = { "%s" }, click = "v:lua.ScFa", maxwidth = 2 },

          { text = { builtin.lnumfunc }, click = "v:lua.scla" },
          {
            text = { " ", builtin.foldfunc, " " },
            condition = { builtin.not_empty, true, builtin.not_empty },
            click = "v:lua.scfa",
          },
        },
      })
    end,
  },

  -- BETTER fold
  -- TODO: make a PR
  {
    "kevinhwang91/nvim-ufo",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    opts = {
      open_fold_hl_timeout = 0,
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "*",
        callback = function()
          if vim.bo.filetype == "snacks_dashboard" then
            vim.wo.foldcolumn = "0" -- Use window-local setting
          else
            vim.wo.foldcolumn = "1"
          end
        end,
      })

      vim.opt.fillchars:append({
        foldopen = "",
        foldsep = " ",
        foldclose = "",
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }
      local language_servers = vim.lsp.get_clients() -- or list servers manually like {'gopls', 'clangd'}
      for _, ls in ipairs(language_servers) do
        require("lspconfig")[ls].setup({
          capabilities = capabilities,
        })
      end
      local handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local suffix = (" 󰁂 %d "):format(endLnum - lnum)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end
      opts.fold_virt_text_handler = handler
      require("ufo").setup(opts)
    end,
  },

  -- better search
  {
    "MagicDuck/grug-far.nvim",
    lazy = true,
    enabled = false,
    opts = { headerMaxWidth = 80 },
    event = "VeryLazy",
    cmd = "GrugFar",
    keys = {
      {
        "<leader>/",
        function()
          local grug = require("grug-far")
          local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
          grug.open({
            transient = true,
            prefills = {
              filesFilter = ext and ext ~= "" and "*." .. ext or nil,
            },
          })
        end,
        mode = { "n", "v" },
        desc = "Search and Replace",
      },
    },
  },
}
