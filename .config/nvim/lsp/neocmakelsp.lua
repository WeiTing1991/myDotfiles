return {
  cmd = { "neocmakelsp", "--stdio" },
  filetypes = { "cmake" },
  single_file_support = true,
  init_options = {
    scan_cmake_in_package = false,
    semantic_token = false,
  },
  settings = {
    neocmakelsp = {
      lint = { enable = true, lineLength = 120 },
      format = { enable = true },
      scan_cmake_in_package = false,
    },
  },
}
