return {
  root_markers = {
    "compile_commands.json",
    "compile_flags.txt",
    "configure.ac",
    "Makefile",
    "meson.build",
    "build.ninja",
    ".git",
    "CMakeLists.txt",
  },
  capabilities = {
    offsetEncoding = { "utf-16" },
  },
  cmd = {
    "clangd",
    "--background-index",
    "--header-insertion=iwyu",
    "--completion-style=detailed",
    "--function-arg-placeholders",
    "--fallback-style=llvm",
  },
  init_options = {
    usePlaceholders = true,
    completeUnimported = true,
    clangdFileStatus = true,
    semanticHighlighting = true,
  },
}
