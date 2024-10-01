return {
  {
    "yetone/avante.nvim",
    opts = {},
    build = "make",
    dependencies = {
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
    },
  },
}

--[[
Leaderaa	show sidebar
Leaderar	refresh sidebar
Leaderae	edit selected blocks
co	choose ours
ct	choose theirs
ca	choose all theirs
c0	choose none
cb	choose both
cc	choose cursor
]x	move to previous conflict
[x	move to next conflict
[[	jump to previous codeblocks (results window)
]]
-- jump to next codeblocks (results windows)
-- ]]
