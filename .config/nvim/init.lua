vim.cmd [[
      if exists("g:gui_widgets")
      call GuiWidgetClientAttach(g:goneovim_channel)
      endif
      ]]
require "core"
