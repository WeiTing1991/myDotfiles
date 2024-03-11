return {

  {
    "michaelb/sniprun",
    branch = "master",

    build = "sh install.sh",
    -- do 'sh install.sh 1' if you want to force compile locally
    -- have to make sure intall rustup and cargo toolchain first
    config = function()
      require("sniprun").setup({
        interpreter_options = {
          Rust_original = {
            compile = "rustc",
          },
        },
      })
    end,
  },
}
