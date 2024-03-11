return {
	-- which loads which-key after all the UI elements are loaded. Events can be
	-- normal autocommands events (:help autocomd-events).

	{
		"folke/which-key.nvim",
		event = "VeryLazy", -- Sets the loading event to 'VeryLazy'
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 500
		end,
		config = function() -- This is the function that runs, AFTER loading
			require("which-key").setup()

			-- Document existing key chains
			require("which-key").register({
			--	["<leader>c"] = { name = "[C]ode", _ = "which_key_ignore" },
			--	["<leader>d"] = { name = "[D]ocument", _ = "which_key_ignore" },
			--	["<leader>r"] = { name = "[R]ename", _ = "which_key_ignore" },
			--	["<leader>s"] = { name = "[S]earch", _ = "which_key_ignore" },
			--	["<leader>w"] = { name = "[W]orkspace", _ = "which_key_ignore" },
			})
		end,
	},
}
