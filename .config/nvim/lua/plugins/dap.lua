-- https://github.com/MariaSolOs/dotfiles/blob/main/.config/nvim/lua/plugins/dap.lua

-- local arrows = require("icon").arrows
-- -- Set up icons.
-- local icons = {
--   Stopped = { "", "DiagnosticWarn", "DapStoppedLine" },
--   Breakpoint = "",
--   BreakpointCondition = "",
--   BreakpointRejected = { "", "DiagnosticError" },
--   LogPoint = arrows.right,
-- }
--
-- for name, sign in pairs(icons) do
--   sign = type(sign) == "table" and sign or { sign }
--   vim.fn.sign_define("Dap" .. name, {
--     -- stylua: ignore
--     text = sign[1] --[[@as string]] .. ' ',
--     texthl = sign[2] or "DiagnosticInfo",
--     linehl = sign[3],
--     numhl = sign[3],
--   })
-- end
-- NOTE:
-- install debuggers manually for now, e.g., for python:

return {
  {
    "mfussenegger/nvim-dap",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      {
        "rcarriga/nvim-dap-ui",

        lazy = true,
        event = "VeryLazy",
        dependencies = {
          "nvim-neotest/nvim-nio",
        },
      },
    },
    keys = {
      {
        "<F5>",
        function()
          require("dap").continue()
        end,
        desc = "Debug: Start/Continue",
      },
      {
        "<F10>",
        function()
          require("dap").step_over()
        end,
        desc = "Debug: Step Over",
      },
      {
        "<F11>",
        function()
          require("dap").step_into()
        end,
        desc = "Debug: Step Into",
      },
      {
        "<S-F11>",
        function()
          require("dap").step_out()
        end,
        desc = "Debug: Step Out",
      },
      {
        "<F9>",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Debug: Toggle Breakpoint",
      },
      {
        "<F6>",
        function()
          require("dapui").toggle()
        end,
        desc = "Debug: See last session result.",
      },
      {
        "<S-F5>",
        function()
          require("dap").terminate()
        end,
        desc = "Debug: Stop",
      },
      -- {
      --   "<leader>B",
      --   function()
      --     require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
      --   end,
      --   desc = "Debug: Set Breakpoint",
      -- },
      -- -- Toggle to see last session result. Without this, you can't see session output in case of unhandled exception.
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      -- Have to install the debuggers manually for now

      -- Auto-open UI when debugging starts
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      -- Auto-close UI when debugging ends
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end

      -- Change breakpoint icons
      vim.api.nvim_set_hl(0, "DapBreak", { fg = "#e51400" })
      vim.api.nvim_set_hl(0, "DapStop", { fg = "#ffcc00" })
      local breakpoint_icons = vim.g.have_nerd_font
          and {
            Breakpoint = "",
            BreakpointCondition = "",
            BreakpointRejected = "",
            LogPoint = "",
            Stopped = "",
          }
        or {
          Breakpoint = "●",
          BreakpointCondition = "⊜",
          BreakpointRejected = "⊘",
          LogPoint = "◆",
          Stopped = "⭔",
        }
      for type, icon in pairs(breakpoint_icons) do
        local tp = "Dap" .. type
        local hl = (type == "Stopped") and "DapStop" or "DapBreak"
        vim.fn.sign_define(tp, { text = icon, texthl = hl, numhl = hl })
      end

      dapui.setup({
        expand_lines = true,
        controls = { enabled = false }, -- no extra play/step buttons
        floating = { border = "rounded" },
        -- Set dapui window
        render = {
          max_type_length = 60,
          max_value_lines = 200,
        },
        layouts = {
          {
            elements = {
              { id = "scopes", size = 0.7 },
              { id = "stacks", size = 0.25 },
              -- { id = "watches", size = 0.3 },
              -- { id = "repl", size = 0.3 },
            },
            size = 50,
            position = "bottom",
          },
        },
      })

      -- CSharp configurations
      -- Install netcoredbg
      -- Not working properly on mac
      -- local netcoredbg_adapter = {
      --   type = "executable",
      --   command = vim.fn.expand("~/netcoredbg/netcoredbg"),
      --   args = { "--interpreter=vscode" },
      -- }
      -- dap.adapters.netcoredbg = netcoredbg_adapter -- needed for normal debugging
      -- dap.adapters.coreclr = netcoredbg_adapter -- needed for unit test debugging
      --
      -- dap.configurations.cs = {
      --   {
      --     type = "coreclr",
      --     name = "Launch",
      --     request = "launch",
      --     program = function()
      --       -- Auto-find dll in any .NET version
      --       local cwd = vim.fn.getcwd()
      --       local dll_pattern = cwd .. "/bin/Debug/net*/**.dll"
      --       local dll_files = vim.fn.glob(dll_pattern, false, true)
      --       if #dll_files > 0 then
      --         return dll_files[1]
      --       end
      --       return vim.fn.input("Path to dll: ", cwd .. "/bin/Debug/net8.0", "file")
      --     end,
      --     -- cwd = vim.fn.getcwd(),
      --     -- stopAtEntry = true,
      --     -- console = "integratedTerminal",
      --   },
      -- }

      -- c/c++
      -- Install codelldb
      dap.adapters.codelldb = {
        type = "executable",
        command = vim.fn.stdpath("data") .. "/mason/bin/codelldb",
        -- detached = false,
      }
      dap.configurations.cpp = {
        {
          name = "Launch file",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
        },
      }
      dap.configurations.c = dap.configurations.cpp
    end,
  },

  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    lazy = true,
    event = "VeryLazy",
    dependencies = {
      "mfussenegger/nvim-dap",
    },
    -- https://codeberg.org/mfussenegger/nvim-dap-python
    config = function()
      require("dap-python").setup("uv")
    end,
  },
}
