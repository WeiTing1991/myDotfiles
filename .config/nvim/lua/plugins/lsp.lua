return {
  -- Mason: tool installer (cmd-only, no eager load)
  {
    "mason-org/mason.nvim",
    cmd = "Mason",
    opts = {
      registries = {
        "github:mason-org/mason-registry",
        "github:Crashdummyy/mason-registry",
      },
    },
  },

  -- Auto-install tools after UI
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    event = "VeryLazy",
    dependencies = { "mason-org/mason.nvim" },
    opts = {
      ensure_installed = {
        -- LSP servers
        "lua-language-server", "bash-language-server", "marksman",
        "json-lsp", "yaml-language-server", "taplo",
        "dockerfile-language-server", "docker-compose-language-service",
        "basedpyright", "ruff",
        "clangd", "neocmakelsp", "lemminx",
        -- Formatters
        "stylua", "prettier", "shfmt", "clang-format", "csharpier",
        -- Linters
        "actionlint",
      },
      run_on_start = false,
      start_delay = 10,
    },
  },

  -- LSP progress indicator
  {
    "j-hui/fidget.nvim",
    event = "LspAttach",
    opts = { notification = { window = { winblend = 0 } } },
  },

  -- Lua dev support
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },

  -- JSON/YAML schemas
  {
    "b0o/schemastore.nvim",
    lazy = true,
  },

  -- Diagnostic hover
  {
    "WeiTing1991/diagnostic-hover.nvim",
    event = "LspAttach",
    opts = {
      keymap = {
        show_float = "<A-k>",
        hide_float = "<Esc>",
      },
    },
  },

  -- Enable all LSP servers + keymaps (native Neovim 0.11)
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      -- Merge blink.cmp capabilities
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      if pcall(require, "blink.cmp") then
        capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
      end
      capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }

      -- lsp/*.lua auto-discovered by Neovim 0.11
      local servers = {
        "lua_ls", "bashls", "marksman",
        "jsonls", "yamlls", "taplo",
        "dockerls", "docker_compose_language_service",
        "basedpyright", "ruff",
        "clangd", "neocmakelsp", "lemminx",
      }
      for _, server in ipairs(servers) do
        vim.lsp.config(server, { capabilities = capabilities })
        vim.lsp.enable(server)
      end

      -- LSP keymaps on attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("wtc/lsp_attach", { clear = true }),
        callback = function(event)
          local client = vim.lsp.get_client_by_id(event.data.client_id)
          if not client then return end
          local buf = event.buf

          local map = function(keys, func, desc, mode)
            mode = mode or "n"
            vim.keymap.set(mode, keys, func, { buffer = buf, desc = desc })
          end

          -- Hover
          local hover = vim.lsp.buf.hover
          map("K", function()
            return hover({
              max_height = math.floor(vim.o.lines * 0.8),
              max_width = math.floor(vim.o.columns * 0.6),
            })
          end, "Hover Documentation")

          -- Navigation
          if client:supports_method("textDocument/definition") then
            map("gd", vim.lsp.buf.definition, "Go to definition", { "n", "v" })
            map("gD", function()
              vim.cmd("vsplit")
              vim.lsp.buf.definition()
            end, "Go to definition (vsplit)", { "n", "v" })
          end
          map("gi", vim.lsp.buf.implementation, "Goto Implementation")
          map("gr", vim.lsp.buf.references, "Find all References")
          map("gR", function() require("fzf-lua").lsp_references() end, "References (fzf)")
          map("gh", vim.lsp.buf.declaration, "Goto declaration")

          -- Signature help
          if client:supports_method("textDocument/signatureHelp") then
            map("<C-k>", vim.lsp.buf.signature_help, "Signature help", "i")
          end

          -- Code actions & refactoring
          map("g.", vim.lsp.buf.code_action, "Code Action")
          map("<F2>", vim.lsp.buf.rename, "Rename symbol")

          -- Symbols
          map("gO", function() require("fzf-lua").lsp_document_symbols() end, "Document Symbols (fzf)")
          map("<leader>co", "<cmd>Outline<CR>", "Document Outline")
          map("gW", function() require("fzf-lua").lsp_workspace_symbols() end, "Workspace Symbols (fzf)")

          -- Inlay hints toggle
          if client:supports_method("textDocument/inlayHint") then
            map("<leader>th", function()
              vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = buf }))
            end, "Toggle Inlay Hints")
          end
        end,
      })
    end,
  },
}
