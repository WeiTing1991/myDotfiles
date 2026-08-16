return {
  name = "g++ compile (C++17)",
  builder = function()
    return {
      cmd = { "g++" },
      args = {
        "-std=c++17",
        "-Wall",
        "-g",
        vim.fn.expand("%"),
        "-o",
        vim.fn.expand("%:r")
      },
      components = { "default" },
    }
  end,
  condition = {
    filetype = { "cpp" },
  },
}
