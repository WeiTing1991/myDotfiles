return {
  name = "dotnet format",
  builder = function()
    local root = vim.fs.root(0, { ".git" })
    return {
      cmd = { "dotnet" },
      args = { "format" },
      cwd = root,
      components = {
        "default",
        "on_complete_trouble",
      },
    }
  end,
  condition = {
    filetype = { "cs" },
  },
}
