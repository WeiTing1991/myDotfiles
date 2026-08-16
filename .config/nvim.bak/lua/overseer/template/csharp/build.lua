return {
  name = "dotnet build",
  builder = function()
    local sln = vim.fn.input("Solution/project file (leave blank to auto-detect): ", "", "file")
    local args = { "build" }
    local cwd
    if sln ~= "" then
      local abs = vim.fn.fnamemodify(sln, ":p")
      table.insert(args, abs)
      cwd = vim.fn.fnamemodify(abs, ":h")
    else
      local found = vim.fs.find(function(name)
        return name:match("%.sln$") or name:match("%.csproj$")
      end, { upward = true, type = "file", path = vim.fn.expand("%:p:h") })
      cwd = found[1] and vim.fn.fnamemodify(found[1], ":h") or vim.fs.root(0, { ".git" })
    end
    return {
      cmd = { "dotnet" },
      args = args,
      cwd = cwd,
      components = {
        "default",
        { "on_output_quickfix", errorformat = "%f(%l\\,%c): %t%*[^ ] %m" },
        "on_complete_trouble",
      },
    }
  end,
  condition = {
    filetype = { "cs" },
  },
}
