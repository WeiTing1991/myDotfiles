; https://www.autohotkey.com/docs/v2/Hotkeys.htm

#SingleInstance Force ; Prevents duplicate script instances
;#Persistent  ; Keep the script running in the background

TileWidth := A_ScreenWidth / 2
TileHeight := A_ScreenHeight / 2

padding := 10

CapsLock::Ctrl   ; CapsLock → Acts as Ctrl
LWin & Tab::AltTab

!w::  ; Alt + W
{
    Send("!{F4}")  ; Sends Alt + F4 to close the current window
}

#+h::{
; Left Half
    WinMove(padding, padding, TileWidth - padding, A_ScreenHeight - padding, "A")
    MouseMove(A_ScreenWidth / 4, A_ScreenHeight / 2)
}

#+k::{
; Top Half
    WinMove(padding, padding, A_ScreenWidth-padding, TileHeight-padding, "A")
}

#+j::{
; Top bottom
    WinMove(padding, TileHeight-padding, A_ScreenWidth-padding, TileHeight-padding, "A")
}

#+l::{
    WinMove(TileWidth-padding, padding, TileWidth-padding, A_ScreenHeight-padding, "A")
}

#Enter::{
    ; Center Window
    NewWidth := A_ScreenWidth * 0.8  ; 70% of screen width
    NewHeight := A_ScreenHeight * 0.8  ; 70% of screen height
    X := (A_ScreenWidth - NewWidth) / 2
    Y := (A_ScreenHeight - NewHeight) / 2
    WinMove(X, Y, NewWidth, NewHeight, "A")  ; Move and resize the active window
}

#+Enter::{
    if WinActive("A") {
        if WinGetMinMax("A") = 1
            WinRestore("A") ; Restore if maximized
        else
            WinMaximize("A") ; Maximize if not
    }
}

;; app
;; not working good
;#1::
;{
    ;Run('C:\Users\weitingchen\AppData\Local\Microsoft\WindowsApps\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\wt.exe')  ; Replace "Profile 1" with your desired profile name
    ;Run("wt.exe")
    ;WinWait("Terminal Preview")  ; Wait for Chrome to launch
    ;WinActivate("PowerShell")  ; Focus the Chrome window
    ;MouseMove(A_ScreenHeight/2, A_ScreenWidth/2)  ; Move the mouse to the top-left corner
;}

#2::
{
    ; Open Google Chrome with a specific profile
    Run('chrome.exe --profile-directory="eth/usi"')  ; Replace "Profile 1" with your desired profile name
    WinWait("Google Chrome")  ; Wait for Chrome to launch
    WinActivate("Google Chrome")  ; Focus the Chrome window
    MouseMove(A_ScreenHeight/2, A_ScreenWidth/2)  ; Move the mouse to the top-left corner
}

#2::
{
    ; Open Google Chrome with a specific profile
    Run('chrome.exe --profile-directory="mesh"')  ; Replace "Profile 1" with your desired profile name
    WinWait("Google Chrome")  ; Wait for Chrome to launch
    WinActivate("Google Chrome")  ; Focus the Chrome window
    MouseMove(A_ScreenHeight/2, A_ScreenWidth/2)  ; Move the mouse to the top-left corner
}

;#4::
;{
;}