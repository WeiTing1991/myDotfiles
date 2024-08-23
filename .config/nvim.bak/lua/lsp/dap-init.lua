require("dapui").setup()
require("nvim-dap-virtual-text").setup {}

local dap = require "dap"
local dapui = require "dapui"

vim.keymap.set("n", "<F2>", dapui.toggle, { desc = "Debug: See last session result." })

vim.keymap.set("n", "<F5>", dap.step_into, { desc = "Debug: Step Into" })
vim.keymap.set("n", "<F6>", dap.step_over, { desc = "Debug: Step Over" })
vim.keymap.set("n", "<F8>", dap.step_out, { desc = "Debug: Step Out" })

vim.keymap.set("n", "<F4>", dap.repl.open, { desc = "Debug: reopen" })

vim.keymap.set("n", "<F3>", dap.continue, { desc = "Debug: Terminate" })
vim.keymap.set("n", "<leader>b", dap.toggle_breakpoint, { desc = "Debug: Toggle breakpoint" })

vim.keymap.set("n", "<F12>", "<cmd>DapVirtualTextToggle<cr>", { desc = "Debug: Toggle breakpoint" })
-- vim.keymap.set("n", "<leader>B", ":lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
-- vim.keymap.set("n", "<leader>dp", ":lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>")

dap.listeners.after.event_initialized["dapui_config"] = dapui.open
dap.listeners.before.event_terminated["dapui_config"] = dapui.close
dap.listeners.before.event_exited["dapui_config"] = dapui.close

-- install cpp specific config
dap.adapters.codelldb = {
  type = "server",
  port = "${port}",
  executable = {
    -- CHANGE THIS to your path!
    command = vim.fn.stdpath "data" .. "/mason/bin/codelldb",
    args = { "--port", "${port}" },
    -- On windows you may have to uncomment this:
    -- detached = false,
  },
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

-- Install golang specific config
-- Basic debugging keymaps, feel free to change to your liking!
require("dap-go").setup()
vim.keymap.set("n", "<leader>dt", ":lua require'dap-go'.debug_test()<CR>")
