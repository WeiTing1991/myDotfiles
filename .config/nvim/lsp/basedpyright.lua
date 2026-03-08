return {
  settings = {
    basedpyright = {
      analysis = {
        autoSearchPaths = true,
        diagnosticMode = "workspace",
        useLibraryCodeForTypes = true,
        typeCheckingMode = "standard",
        extraPaths = {
          "./src",
          "./lib",
          "./backend",
          "./venv",
          ".env",
        },
      },
      diagnosticSeverityOverrides = {
        reportMissingModuleSource = "none",
        reportMissingTypeStubs = "none",
      },
    },
  },
}
