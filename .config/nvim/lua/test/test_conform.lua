local conform = require "conform"
-- conform.formatters_by_ft = { lua = { "stylua" } }
-- print(vim.inspect(conform.formatters_by_ft))

require "conform".setup {
  formatter_by_ft = {
    lua = { "stylua" },
  },
}

-- Print the entire conform table to see its structure
print(vim.inspect(conform))
