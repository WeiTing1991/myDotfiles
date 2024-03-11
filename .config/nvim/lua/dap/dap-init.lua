local dap = require("dap")
local dapui = require("dapui")

require("dapui").setup()

-- -- add event listeners
-- dap.listeners.after.event_initialized["dapui_config"] = function()
--     dapui.open()
--     vim.cmd("Hardtime disable")
--     vim.cmd("NvimTreeClose")
-- end

dap.listeners.before.attach.dapui_config = function()
  dapui.open()
  vim.cmd("NvimTreeClose")
end
dap.listeners.before.launch.dapui_config = function()
  dapui.open()
end
dap.listeners.before.event_terminated.dapui_config = function()
  dapui.close()
end
dap.listeners.before.event_exited.dapui_config = function()
  dapui.close()
end

-- keymapping
vim.keymap.set("n", "<leader>dt", function()
  dap.toggle_breakpoint()
end, { desc = "Toggle breakpoint" })
vim.keymap.set("n", "<leader>dc", function()
  dap.continue()
end, { desc = "Start or Continue the debugger" })
