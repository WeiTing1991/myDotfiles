return {
  name = "Format all files",
  builder = function()
    -- Find project root (looks for common root markers)
    local root = vim.fs.root(0, { ".git", "lazy-lock.json", "package.json" })

    return {
      cmd = { "stylua" },
      args = { "." },
      cwd = root, -- This sets the working directory to project root
      components = { "default" },
    }
  end,
  condition = {
    filetype = { "lua" },
  },
}
