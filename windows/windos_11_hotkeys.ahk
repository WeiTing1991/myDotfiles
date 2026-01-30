#Requires Autohotkey v2.0+

#SingleInstance Force ; Prevents duplicate script instances

;if not A_IsAdmin {
;   Run('*RunAs "' A_ScriptFullPath '"') ; Requires v1.0.92.01+
;   ExitApp()
;}

TileWidth := A_ScreenWidth / 2
TileHeight := A_ScreenHeight / 2

;padding := 10

CapsLock::Ctrl   ; CapsLock → Acts as Ctrl
;LWin::Alt
;Alt::LWin

;LWin & Tab::AltTab

!w::  ; Alt + W
{
    Send("!{F4}")
}

#+h::
{
    SendEvent("#{Left}") ; Sends Win + Left
    Sleep(300)
    centerMouseInWindow()
}

#+k::
{
    SendEvent("#{Up}") ; Sends Win + Up
    Sleep(300)
    centerMouseInWindow()
}

#+l::
{
    SendEvent("#{Right}") ; Sends Win + Right
    Sleep(300)
    centerMouseInWindow()
}

#+j::
{
    SendEvent("#{Down}") ; Sends Win + Down
    Sleep(300)
    centerMouseInWindow()
}

#+Enter::{
    ; Center Window (95% size)
    NewWidth := A_ScreenWidth * 0.95
    NewHeight := A_ScreenHeight * 0.95
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")
    Sleep(100)
    centerMouseInWindow()
}

#^Enter::{
    ; Center Window (50% size)
    NewWidth := A_ScreenWidth * 0.50
    NewHeight := A_ScreenHeight * 0.50
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")
    Sleep(100)
    centerMouseInWindow()
}

#Enter::{
    if WinActive("A") {
        if WinGetMinMax("A") = 1
            WinRestore("A") ; Restore if maximized
        else
            WinMaximize("A") ; Maximize if not
        Sleep(300)
        centerMouseInWindow()
    }
}

; Desktop switching
#^j::
{
    SendEvent("#^{Left}")
}

#^k::
{
    SendEvent("#^{Right}")
}

; === FUNCTIONS ===

centerMouseInWindow() {
    hwnd := WinExist("A")
    if !hwnd
        return

    Sleep(100)

    WinGetPos(&winX, &winY, &winW, &winH, "A")

    if (winW <= 0 || winH <= 0)
        return

    RECT := Buffer(16, 0)
    DllCall("GetClientRect", "Ptr", hwnd, "Ptr", RECT)

    clientWidth := NumGet(RECT, 8, "Int")
    clientHeight := NumGet(RECT, 12, "Int")

    if (clientWidth <= 0 || clientHeight <= 0) {
        MouseMove(winX + winW//2, winY + winH//2, 0)
        return
    }

    POINT := Buffer(8, 0)
    NumPut("Int", 0, POINT, 0)
    NumPut("Int", 0, POINT, 4)
    DllCall("ClientToScreen", "Ptr", hwnd, "Ptr", POINT)

    clientX := NumGet(POINT, 0, "Int")
    clientY := NumGet(POINT, 4, "Int")

    centerX := clientX + clientWidth//2
    centerY := clientY + clientHeight//2

    MouseMove(centerX, centerY, 0)
}

cycleWindows(exeName) {
    idList := WinGetList("ahk_exe " exeName)

    if (idList.Length = 0)
        return

    if (idList.Length = 1) {
        if (WinGetMinMax("ahk_id " idList[1]) = -1)
            WinRestore("ahk_id " idList[1])
        WinActivate("ahk_id " idList[1])
        Sleep(150)
        centerMouseInWindow()
        return
    }

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

    if (! isCurrentlyActive) {
        nextId := idList[1]
    } else {
        nextIdx := (currentIdx = idList.Length) ? 1 : currentIdx + 1
        nextId := idList[nextIdx]
    }

    if (WinGetMinMax("ahk_id " nextId) = -1)
        WinRestore("ahk_id " nextId)

    WinActivate("ahk_id " nextId)
    Sleep(150)
    centerMouseInWindow()
}

; === APP SHORTCUTS ===

#1::cycleWindows("WindowsTerminal.exe")
#4::cycleWindows("chrome.exe")
#7::cycleWindows("Code.exe")

#+1::Run("wt")
#+4::Run("chrome.exe")
#+7::Run("code.exe")


