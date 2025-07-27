-- app lancher

local function mouse_focus(app_name)
  local app = hs.application.find(app_name)
  if app then
    -- App is already running, focus and move mouse
    app:activate()
    local window = app:mainWindow()
    if window then
      local frame = window:frame()
      hs.mouse.absolutePosition({ x = frame.x + frame.w / 2, y = frame.y + frame.h / 2 })
    end
  else
    -- App not running, launch it
    hs.application.open(app_name)
  end
end

local function mouse_focus_cycle(app_name)
  local app = hs.application.find(app_name)

  if app then
    local windows = app:allWindows()

    if #windows > 1 then
      -- Multiple windows - cycle through them
      local focusedWindow = app:focusedWindow()
      local nextWindow = nil

      for i, window in ipairs(windows) do
        if window == focusedWindow then
          nextWindow = windows[(i % #windows) + 1]
          break
        end
      end
      if not nextWindow then
        nextWindow = windows[1]
      end
      -- smalle bugs
      nextWindow:focus()
      local frame = nextWindow:frame()
      hs.mouse.absolutePosition({ x = frame.x + frame.w / 2, y = frame.y + frame.h / 2 })
    else
      -- Single window
      app:activate()
      local window = app:mainWindow()
      if window then
        local frame = window:frame()
        hs.mouse.absolutePosition({ x = frame.x + frame.w / 2, y = frame.y + frame.h / 2 })
      end
    end
  else
    -- Launch app
    hs.application.open(app_name)
  end
end

hs.hotkey.bind({ "cmd" }, "1", function()
  mouse_focus("WezTerm")
end)

hs.hotkey.bind({ "cmd" }, "2", function()
  mouse_focus_cycle("Brave Browser")
end)

hs.hotkey.bind({ "cmd" }, "3", function()
  mouse_focus("Obsidian")
end)


hs.hotkey.bind({ "cmd" }, "7", function()
  mouse_focus("Visual Studio Code")
end)

hs.hotkey.bind({ "cmd" }, "8", function()
  mouse_focus("Zed")
end)

-- Auto-reload config when file changes
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert.show("Config loaded")
