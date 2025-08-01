; https://www.autohotkey.com/docs/v2/Hotkeys.htm

; Auto-elevate the script

#SingleInstance Force ; Prevents duplicate script instances

if not A_IsAdmin {
   Run('*RunAs "' A_ScriptFullPath '"') ; Requires v1.0.92.01+
   ExitApp()
}

;#Persistent  ; Keep the script running in the background

TileWidth := A_ScreenWidth / 2
TileHeight := A_ScreenHeight / 2

padding := 10

CapsLock::Ctrl   ; CapsLock → Acts as Ctrl
LWin & Tab::AltTab

!w::  ; Alt + W
{
    Send("!{F4}")
}

#+h::
{
    SendEvent("#{Left}") ; Sends Win + Left
}

; Win + Shift + K -> Snap window to top half
#+k::
{
    SendEvent("#{Up}") ; Sends Win + Up
}

; Win + Shift + L -> Snap window to right half
#+l::
{
    SendEvent("#{Right}") ; Sends Win + Right
}

#+j::
{
    SendEvent("#{Down}") ; Sends Win + Down
}

#+Enter::{
    ; Center Window
    NewWidth := A_ScreenWidth * 0.95
    NewHeight := A_ScreenHeight * 0.95
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")  ; Move and resize the active window
}

#Enter::{
    if WinActive("A") {
        if WinGetMinMax("A") = 1
            WinRestore("A") ; Restore if maximized
        else
            WinMaximize("A") ; Maximize if not
    }
}

; desktop

#^j::
{
    SendEvent("#^{Left}")
}

#^k::
{
    SendEvent("#^{Right}")
}

; Function to move mouse to center of window
moveMouseToCenter(winTitle) {
    if WinExist(winTitle) {
        WinGetPos(&x, &y, &w, &h, winTitle)
        MouseMove(x + w // 2, y + h // 2)
    }
}

; Function to focus or launch an application
focusApp(appPath, winTitle) {
    if WinExist(winTitle) {
        WinActivate(winTitle)
        WinWaitActive(winTitle, , 2)
        moveMouseToCenter(winTitle)
    } else {
        Run(appPath)
    }
}

; Fixed function to cycle through windows of a given class
cycleWindows(winClass, appPath := "") {
    ; Get list of windows matching the class
    idList := WinGetList("ahk_class " . winClass)

    if (idList.Length > 1) {
        ; Multiple windows found - cycle through them
        activeId := WinGetID("A")
        currentIdx := 0

        ; Find current window in the list
        for i, id in idList {
            if (id = activeId) {
                currentIdx := i
                break
            }
        }

        ; Calculate next window index
        nextIdx := (currentIdx >= idList.Length) ? 1 : currentIdx + 1
        if (currentIdx = 0) ; Current window not in list
            nextIdx := 1

        nextId := idList[nextIdx]

        ; Activate next window
        WinActivate("ahk_id " . nextId)
        WinWaitActive("ahk_id " . nextId, , 2)

        ; Move mouse to center
        if WinExist("ahk_id " . nextId) {
            WinGetPos(&x, &y, &w, &h, "ahk_id " . nextId)
            MouseMove(x + w // 2, y + h // 2)
        }

    } else if (idList.Length = 1) {
        ; Only one window - just focus it
        WinActivate("ahk_id " . idList[1])
        WinWaitActive("ahk_id " . idList[1], , 2)
        moveMouseToCenter("ahk_id " . idList[1])

    } else if (appPath != "") {
        ; No windows found - launch app if path provided
        Run(appPath)
    }
}

; Alternative function using executable name instead of class
cycleWindowsByExe(exeName, appPath := "") {
    idList := WinGetList("ahk_exe " . exeName)

    if (idList.Length > 1) {
        activeId := WinGetID("A")
        currentIdx := 0

        for i, id in idList {
            if (id = activeId) {
                currentIdx := i
                break
            }
        }

        nextIdx := (currentIdx >= idList.Length) ? 1 : currentIdx + 1
        if (currentIdx = 0)
            nextIdx := 1

        nextId := idList[nextIdx]
        WinActivate("ahk_id " . nextId)
        WinWaitActive("ahk_id " . nextId, , 2)
        moveMouseToCenter("ahk_id " . nextId)

    } else if (idList.Length = 1) {
        WinActivate("ahk_id " . idList[1])
        WinWaitActive("ahk_id " . idList[1], , 2)
        moveMouseToCenter("ahk_id " . idList[1])

    } else if (appPath != "") {
        Run(appPath)
    }
}

; === HOTKEYS ===
; Cmd-like hotkeys: use Ctrl (^) on Windows, or customize to your keyboard
#1::focusApp("C:\Program Files\WezTerm\wezterm-gui.exe", "ahk_exe wezterm-gui.exe")
#2::cycleWindows("Chrome_WidgetWin_1") ;
#3::focusApp("C:\Program Files\Obsidian\Obsidian.exe", "ahk_exe Obsidian.exe")
#7::cycleWindowsByExe("Code.exe", "C:\Users\" . A_UserName . "\AppData\Local\Programs\Microsoft VS Code\Code.exe")
; #8::focusApp("C:\Program Files\Zed\Zed.exe", "ahk_exe Zed.exe")
