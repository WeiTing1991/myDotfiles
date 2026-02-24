#Requires Autohotkey v2.0+
#SingleInstance Force ; Prevents duplicate script instances

; --- Screen dimensions for custom window sizing ---
TileWidth := A_ScreenWidth / 2
TileHeight := A_ScreenHeight / 2

; =====================================================
; WINDOW SNAPPING (Vim-style keys)
; These CAN be remapped in PowerToys Keyboard Manager:
;   Win+Shift+H → Win+Left
;   Win+Shift+K → Win+Up
;   Win+Shift+L → Win+Right
;   Win+Shift+J → Win+Down
; BUT the mouse centering part CANNOT, so keep in AHK
; =====================================================

; Win+Shift+H → Snap window left + center mouse
#+h::
{
    SendEvent("#{Left}")
    Sleep(300)
    centerMouseInWindow()
}

; Win+Shift+K → Snap window up + center mouse
#+k::
{
    SendEvent("#{Up}")
    Sleep(300)
    centerMouseInWindow()
}

; Win+Shift+L → Snap window right + center mouse
#+l::
{
    SendEvent("#{Right}")
    Sleep(300)
    centerMouseInWindow()
}

; Win+Shift+J → Snap window down + center mouse
#+j::
{
    SendEvent("#{Down}")
    Sleep(300)
    centerMouseInWindow()
}

; =====================================================
; CUSTOM WINDOW SIZING — AHK ONLY (PowerToys can't do this)
; =====================================================

; Win+Shift+Enter → Center window at 95% of screen
#+Enter::{
    NewWidth := A_ScreenWidth * 0.95
    NewHeight := A_ScreenHeight * 0.95
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")
    Sleep(100)
    centerMouseInWindow()
}

; Win+Ctrl+Enter → Center window at 50% of screen
#^Enter::{
    NewWidth := A_ScreenWidth * 0.50
    NewHeight := A_ScreenHeight * 0.50
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")
    Sleep(100)
    centerMouseInWindow()
}

; Win+Enter → Toggle maximize/restore — AHK ONLY (conditional logic)
#Enter::{
    if WinActive("A") {
        if WinGetMinMax("A") = 1
            WinRestore("A")     ; Restore if currently maximized
        else
            WinMaximize("A")    ; Maximize if not
        Sleep(300)
        centerMouseInWindow()
    }
}

; =====================================================
; VIRTUAL DESKTOP SWITCHING
; These CAN be remapped in PowerToys Keyboard Manager:
;   Win+Ctrl+J → Win+Ctrl+Left
;   Win+Ctrl+K → Win+Ctrl+Right
; No mouse centering needed, so safe to move to PowerToys
; =====================================================

; Win+Ctrl+J → Switch to left desktop
#^j::
{
    SendEvent("#^{Left}")
}

; Win+Ctrl+K → Switch to right desktop
#^k::
{
    SendEvent("#^{Right}")
}

; =====================================================
; FUNCTIONS — AHK ONLY (no equivalent in PowerToys)
; =====================================================

; Centers the mouse cursor in the client area of the active window.
; Uses Windows API calls to get the true client rect (excludes title bar,
; borders) and converts to screen coordinates for accurate positioning.
centerMouseInWindow() {
    hwnd := WinExist("A")           ; Get handle of active window
    if !hwnd
        return

    Sleep(100)                       ; Brief wait for window to settle

    ; Get full window position and size (includes title bar + borders)
    WinGetPos(&winX, &winY, &winW, &winH, "A")
    if (winW <= 0 || winH <= 0)
        return

    ; Get client area dimensions (content area only, no chrome)
    RECT := Buffer(16, 0)
    DllCall("GetClientRect", "Ptr", hwnd, "Ptr", RECT)
    clientWidth := NumGet(RECT, 8, "Int")
    clientHeight := NumGet(RECT, 12, "Int")

    ; Fallback: if client rect fails, center on full window
    if (clientWidth <= 0 || clientHeight <= 0) {
        MouseMove(winX + winW//2, winY + winH//2, 0)
        return
    }

    ; Convert client area origin (0,0) to screen coordinates
    POINT := Buffer(8, 0)
    NumPut("Int", 0, POINT, 0)
    NumPut("Int", 0, POINT, 4)
    DllCall("ClientToScreen", "Ptr", hwnd, "Ptr", POINT)
    clientX := NumGet(POINT, 0, "Int")
    clientY := NumGet(POINT, 4, "Int")

    ; Move mouse to center of client area
    centerX := clientX + clientWidth//2
    centerY := clientY + clientHeight//2
    MouseMove(centerX, centerY, 0)
}

; Cycles through all open windows of a given executable.
; If only one window exists, activates it (restoring if minimized).
; If multiple exist, activates the next one in the list (round-robin).
; Always centers mouse in the newly activated window.
cycleWindows(exeName) {
    idList := WinGetList("ahk_exe " exeName)   ; Get all windows for this exe
    if (idList.Length = 0)
        return

    ; Only one window — just activate it
    if (idList.Length = 1) {
        if (WinGetMinMax("ahk_id " idList[1]) = -1)
            WinRestore("ahk_id " idList[1])     ; Restore if minimized
        WinActivate("ahk_id " idList[1])
        Sleep(150)
        centerMouseInWindow()
        return
    }

    ; Multiple windows — find current and cycle to next
    activeId := WinGetID("A")
    currentIdx := 0
    isCurrentlyActive := false
    for i, id in idList {
        if (id = activeId) {
            currentIdx := i
            isCurrentlyActive := true
            break
        }
    }

    ; If this exe isn't the active app, jump to its first window
    if (! isCurrentlyActive) {
        nextId := idList[1]
    } else {
        ; Wrap around to first window if at the end
        nextIdx := (currentIdx = idList.Length) ? 1 : currentIdx + 1
        nextId := idList[nextIdx]
    }

    if (WinGetMinMax("ahk_id " nextId) = -1)
        WinRestore("ahk_id " nextId)            ; Restore if minimized
    WinActivate("ahk_id " nextId)
    Sleep(150)
    centerMouseInWindow()
}

; =====================================================
; APP SHORTCUTS — AHK ONLY (PowerToys can't launch/cycle apps)
; =====================================================

; Win+1 → Cycle through Windows Terminal windows
#1::cycleWindows("WindowsTerminal.exe")
; Win+Shift+1 → Launch new Windows Terminal
#+1::Run("wt")

; Win+Shift+7 → Launch new VS Code
#7::cycleWindows("Code.exe")
#+7::Run("code.exe")

; Win- → Cycle through Chrome windows
#-::cycleWindows("chrome.exe")
;#+::cycleWindows("chrome.exe")
