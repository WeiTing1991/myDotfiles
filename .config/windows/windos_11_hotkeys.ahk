; https://www.autohotkey.com/docs/v2/Hotkeys.htm


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

; Move mouse to center of a window
moveMouseToClientCenter(winTitle) {
    hwnd := WinExist(winTitle)
    if !hwnd {
        MsgBox("Window not found: " winTitle)
        return
    }

    ; Get window position
    WinGetPos(&winX, &winY, &winW, &winH, winTitle)

    ; Initialize buffer for GetClientRect
    rc := Buffer(16, 0)  ; AutoHotkey v2 syntax for buffer

    success := DllCall("User32.dll\GetClientRect", "Ptr", hwnd, "Ptr", rc)
    if !success {
        MsgBox("GetClientRect failed")
        return
    }

    ; Get client area dimensions
    clientW := NumGet(rc, 8, "Int")   ; right - left (left is always 0)
    clientH := NumGet(rc, 12, "Int")  ; bottom - top (top is always 0)

    ; Calculate window frame offsets
    offsetX := (winW - clientW) // 2  ; Use / instead of //
    offsetY := winH - clientH - offsetX

    ; Calculate center position
    centerX := winX + offsetX + clientW // 2  ; Use / instead of //
    centerY := winY + offsetY + clientH // 2  ; Use / instead of //

    ; Move mouse to center
    MouseMove(centerX, centerY, 0)
}

; Simple version - move to window center (without client area calculation)
moveMouseToCenter(winTitle) {
    if !WinExist(winTitle) {
        MsgBox("Window not found: " winTitle)
        return
    }

    WinGetPos(&winX, &winY, &winW, &winH, winTitle)
    MouseMove(winX + winW//2, winY + winH//2, 0)
}

; Function to focus or launch an application
focusApp(appPath, winTitle) {
    if WinExist(winTitle) {
        WinActivate(winTitle)
        moveMouseToCenter(winTitle)  ; Fixed function name
    } else {
        Run(appPath)
    }
}

; Cycle through windows of a given executable
cycleWindows(exeName, appPath := "") {
    idList := WinGetList("ahk_exe " exeName)

    if (idList.Length > 1) {
        ; Multiple windows found - cycle through them
        activeId := WinGetID("A")
        idx := 0

        ; Find current window index
        for i, id in idList {
            if (id = activeId) {
                currentIdx := i
                break
            }
        }

        ; Calculate next window index
        nextIdx := idx ? (idx = idList.Length ? 1 : idx + 1) : 1
        nextId := idList[nextIdx]

        ; Activate next window
        WinActivate("ahk_id " nextId)
        WinWaitActive("ahk_id " nextId)
        moveMouseToClientCenter("ahk_id " nextId)

    } else if idList.Length = 1 {
        ; Only one window - just focus and center mouse
        WinActivate("ahk_id " idList[1])
        moveMouseToClientCenter("ahk_id " idList[1])

    } else if appPath {
        ; No windows found - launch app
        Run(appPath)
    }
}

; === HOTKEYS ===
; Windows key + number combinations
#1::focusApp("C:\Program Files\WezTerm\wezterm-gui.exe", "ahk_exe wezterm-gui.exe")
#2::cycleWindows("chrome.exe", "C:\Program Files\Google\Chrome\Application\chrome.exe")
#7::cycleWindows("Code.exe", "C:\Users\weichen34\AppData\Local\Programs\Microsoft VS Code\Code.exe")

; Uncomment these if you want them:
; #3::focusApp("C:\Program Files\Obsidian\Obsidian.exe", "ahk_exe Obsidian.exe")
; #8::focusApp("C:\Program Files\Zed\Zed.exe", "ahk_exe Zed.exe")

