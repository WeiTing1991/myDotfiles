return {
  filetypes = { "json", "jsonc" },
  settings = {
    json = {
      validate = { enable = true },
      schemas = require("schemastore").json.schemas(),
    },
  },
}
