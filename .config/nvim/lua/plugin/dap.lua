-- https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/plugins/dap.lua

local arrows = require("icon").arrows
-- Set up icons.
local icons = {
  Stopped = { "", "DiagnosticWarn", "DapStoppedLine" },
  Breakpoint = "",
  BreakpointCondition = "",
  BreakpointRejected = { "", "DiagnosticError" },
  LogPoint = arrows.right,
}

for name, sign in pairs(icons) do
  sign = type(sign) == "table" and sign or { sign }
  vim.fn.sign_define("Dap" .. name, {
    -- stylua: ignore
    text = sign[1] --[[@as string]] .. ' ',
    texthl = sign[2] or "DiagnosticInfo",
    linehl = sign[3],
    numhl = sign[3],
  })
end

return {
  -- DEBUGGER
  {
    "mfussenegger/nvim-dap",
    lazy = true,
    event = "BufEnter",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "nvim-neotest/nvim-nio",
      "theHamsta/nvim-dap-virtual-text",
      "jay-babu/mason-nvim-dap.nvim",

      -- Add own debuggers here
      -- "leoluz/nvim-dap-go",
      -- "mfussenegger/nvim-dap-python",
      -- lua
      {
        "jbyuki/one-small-step-for-vimkind",
      },
    },
    keys = {
      -- Basic debugging keymaps, feel free to change to your liking!
      {
        "<F5>",
        function()
          require("dap").continue()
        end,
        desc = "Debug: Start/Continue",
      },
    },
    config = function()
      local dap = require "dap"
      local dapui = require "dapui"

      -- doesnt work with mason-dap
      -- require("mason-nvim-dap").setup {
      --   automatic_installation = true,
      --   handlers = {},
      --   ensure_installed = require("lsp.debugger")
      -- }

      dapui.setup()

      -- Automatically open the UI when a new debug session is created.
      dap.listeners.after.event_initialized["dapui_config"] = dapui.open
      dap.listeners.before.event_terminated["dapui_config"] = dapui.close
      dap.listeners.before.event_exited["dapui_config"] = dapui.close

      -- csharp configurations
      dap.adapters.coreclr = {
        type = "executable",
        command = vim.fn.stdpath "data" .. "/mason/bin/netcoredbg",
        args = { "--interpreter=vscode" },
      }
      dap.configurations.cs = {
        {
          type = "coreclr",
          name = "launch - netcoredbg",
          request = "launch",
          program = function()
            return vim.fn.input("Path to dll", vim.fn.getcwd() .. "/bin/Debug/", "file")
          end,
        },
      }
      -- Lua configurations.
      dap.adapters.nlua = function(callback, config)
        callback { type = "server", host = config.host or "127.0.0.1", port = config.port or 8086 }
      end
      dap.configurations["lua"] = {
        {
          type = "nlua",
          request = "attach",
          name = "Attach to running Neovim instance",
          program = function()
            pcall(require("osv").launch { port = 8086 })
          end,
        },
      }
    end,
  },
}
