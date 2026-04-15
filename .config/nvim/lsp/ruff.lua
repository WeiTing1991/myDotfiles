return {
  on_attach = function(client, bufnr)
    -- Disable hover in ruff to avoid conflicts with basedpyright
    client.server_capabilities.hoverProvider = false
  end,
}
