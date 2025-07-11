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

;; app
;; not working good

;#2::
;{
;    ; Open Google Chrome with a specific profile
;    Run('chrome.exe --profile-directory="eth/usi"')  ; Replace "Profile 1" with your desired profile name
;    WinWait("Google Chrome")  ; Wait for Chrome to launch
;    WinActivate("Google Chrome")  ; Focus the Chrome window
;    MouseMove(A_ScreenHeight/2, A_ScreenWidth/2)  ; Move the mouse to the top-left corner
;}

