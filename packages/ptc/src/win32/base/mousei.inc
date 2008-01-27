{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2006  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Constructor TWin32Mouse.Create(window : HWND; thread : DWord; multithreaded : Boolean; EventQueue : TEventQueue; FullScreen : Boolean; ConsoleWidth, ConsoleHeight : Integer);

Begin
  Inherited Create(window, thread);

  FEventQueue := EventQueue;

  FFullScreen := FullScreen;
  FConsoleWidth := ConsoleWidth;
  FConsoleHeight := ConsoleHeight;

  FPreviousMousePositionSaved := False;

  { enable buffering }
  FEnabled := True;
End;

Procedure TWin32Mouse.SetWindowArea(WindowX1, WindowY1, WindowX2, WindowY2 : Integer);

Begin
  FWindowX1 := WindowX1;
  FWindowY1 := WindowY1;
  FWindowX2 := WindowX2;
  FWindowY2 := WindowY2;
End;

Procedure TWin32Mouse.enable;

Begin
  { enable buffering }
  FEnabled := True;
End;

Procedure TWin32Mouse.disable;

Begin
  { disable buffering }
  FEnabled := False;
End;

Function TWin32Mouse.WndProc(hWnd : HWND; message : DWord; wParam : WPARAM; lParam : LPARAM) : LRESULT;

Var
  fwKeys : Integer;
  xPos, yPos : Integer;
  LButton, MButton, RButton : Boolean;
  TranslatedXPos, TranslatedYPos : Integer;
  PTCMouseButtonState : TPTCMouseButtonState;
  WindowRect : RECT;

  button : TPTCMouseButton;
  before, after : Boolean;
  cstate : TPTCMouseButtonState;

Begin
  Result := 0;
  { check enabled flag }
  If Not FEnabled Then
    Exit;

  If (message = WM_MOUSEMOVE) Or
     (message = WM_LBUTTONDOWN) Or (message = WM_LBUTTONUP) Or (message = WM_LBUTTONDBLCLK) Or
     (message = WM_MBUTTONDOWN) Or (message = WM_MBUTTONUP) Or (message = WM_MBUTTONDBLCLK) Or
     (message = WM_RBUTTONDOWN) Or (message = WM_RBUTTONUP) Or (message = WM_RBUTTONDBLCLK) Then
  Begin
    fwKeys := wParam; {MK_LBUTTON or MK_MBUTTON or MK_RBUTTON or MK_CONTROL or MK_SHIFT}
    xPos := lParam And $FFFF;
    yPos := (lParam Shr 16) And $FFFF;

    LButton := (fwKeys And MK_LBUTTON) <> 0;
    MButton := (fwKeys And MK_MBUTTON) <> 0;
    RButton := (fwKeys And MK_RBUTTON) <> 0;

    If Not FFullScreen Then
    Begin
      GetClientRect(hWnd, WindowRect);

      FWindowX1 := WindowRect.left;
      FWindowY1 := WindowRect.top;
      FWindowX2 := WindowRect.right - 1;
      FWindowY2 := WindowRect.bottom - 1;
    End;

    If (xPos >= FWindowX1) And (yPos >= FWindowY1) And
       (xPos <= FWindowX2) And (yPos <= FWindowY2) Then
    Begin
      If FWindowX2 <> FWindowX1 Then
        TranslatedXPos := (xPos - FWindowX1) * (FConsoleWidth  - 1) Div (FWindowX2 - FWindowX1)
      Else { avoid div by zero }
        TranslatedXPos := 0;

      If FWindowY2 <> FWindowY1 Then
        TranslatedYPos := (yPos - FWindowY1) * (FConsoleHeight - 1) Div (FWindowY2 - FWindowY1)
      Else { avoid div by zero }
        TranslatedYPos := 0;

      { Just in case... }
      If TranslatedXPos < 0 Then
        TranslatedXPos := 0;
      If TranslatedYPos < 0 Then
        TranslatedYPos := 0;
      If TranslatedXPos >= FConsoleWidth Then
        TranslatedXPos := FConsoleWidth - 1;
      If TranslatedYPos >= FConsoleHeight Then
        TranslatedYPos := FConsoleHeight - 1;

      If Not LButton Then
        PTCMouseButtonState := []
      Else
        PTCMouseButtonState := [PTCMouseButton1];

      If RButton Then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton2];

      If MButton Then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton3];

      If Not FPreviousMousePositionSaved Then
      Begin
        FPreviousMouseX := TranslatedXPos; { first DeltaX will be 0 }
        FPreviousMouseY := TranslatedYPos; { first DeltaY will be 0 }
        FPreviousMouseButtonState := [];
      End;

      { movement? }
      If (TranslatedXPos <> FPreviousMouseX) Or (TranslatedYPos <> FPreviousMouseY) Then
        FEventQueue.AddEvent(TPTCMouseEvent.Create(TranslatedXPos, TranslatedYPos, TranslatedXPos - FPreviousMouseX, TranslatedYPos - FPreviousMouseY, FPreviousMouseButtonState));

      { button presses/releases? }
      cstate := FPreviousMouseButtonState;
      For button := Low(button) To High(button) Do
      Begin
        before := button In FPreviousMouseButtonState;
        after := button In PTCMouseButtonState;
        If after And (Not before) Then
        Begin
          { button was pressed }
          cstate := cstate + [button];
          FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, True, button));
        End
        Else
          If before And (Not after) Then
          Begin
            { button was released }
            cstate := cstate - [button];
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, button));
          End;
      End;

      FPreviousMouseX := TranslatedXPos;
      FPreviousMouseY := TranslatedYPos;
      FPreviousMouseButtonState := PTCMouseButtonState;
      FPreviousMousePositionSaved := True;
    End;
  End;
End;
