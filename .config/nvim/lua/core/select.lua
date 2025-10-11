local M = {}

function M.select(picker_fn)
  local original_select = vim.ui.select

  vim.ui.select = function(items, opts, on_choice)
    opts = opts or {}

    -- Format items for Snacks picker
    local picker_items = {}
    for i, item in ipairs(items) do
      local text = opts.format_item and opts.format_item(item) or tostring(item)
      table.insert(picker_items, {
        idx = i,
        text = text,
        value = item,
      })
    end

    -- Use Snacks picker with correct API
    Snacks.picker({
      title = opts.prompt or "Select",
      items = picker_items,
      format = function(item)
        return { { item.text } }
      end,
      confirm = function(picker, item)
        picker:close()
        if item and on_choice then
          on_choice(item.value, item.idx)
        elseif on_choice then
          on_choice(nil, nil)
        end
      end,
    })
  end

  picker_fn()

  vim.defer_fn(function()
    vim.ui.select = original_select
  end, 100)
end

return M
