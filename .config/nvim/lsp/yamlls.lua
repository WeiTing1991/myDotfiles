return {
  filetypes = { "yaml", "yml" },
  settings = {
    yaml = {
      schemastore = { enable = false, url = "" },
      schemas = vim.tbl_deep_extend("force", require("schemastore").yaml.schemas(), {
        ["https://json.schemastore.org/github-workflow.json"] = "/.github/workflows/*.{yml,yaml}",
        ["https://json.schemastore.org/github-action.json"] = "action.{yml,yaml}",
      }),
    },
  },
}
