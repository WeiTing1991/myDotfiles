local constants = require("overseer.constants")
local STATUS = constants.STATUS

return {
  desc = "Open trouble on failure, close on success",
  constructor = function()
    return {
      on_complete = function(self, task, status)
        vim.schedule(function()
          if status == STATUS.FAILURE then
            require("trouble").open({ mode = "qflist", focus = true })
          elseif status == STATUS.SUCCESS then
            require("trouble").close()
            vim.notify(task.name .. " succeeded!", vim.log.levels.INFO)
          end
        end)
      end,
    }
  end,
}
