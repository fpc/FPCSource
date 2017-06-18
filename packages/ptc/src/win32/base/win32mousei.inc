{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2007, 2009, 2010, 2013, 2016  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

constructor TWin32Mouse.Create(AWindow: HWND; AThread: DWord; AMultithreaded: Boolean; AEventQueue: TEventQueue;
     AFullScreen: Boolean; AConsoleWidth, AConsoleHeight: Integer);
begin
  inherited Create(AWindow, AThread);

  FEventQueue := AEventQueue;

  FFullScreen := AFullScreen;
  FConsoleWidth := AConsoleWidth;
  FConsoleHeight := AConsoleHeight;

  FPreviousMousePositionSaved := False;

  { enable buffering }
  FEnabled := True;
end;

procedure TWin32Mouse.SetWindowArea(AWindowX1, AWindowY1, AWindowX2, AWindowY2: Integer);
begin
  FWindowX1 := AWindowX1;
  FWindowY1 := AWindowY1;
  FWindowX2 := AWindowX2;
  FWindowY2 := AWindowY2;
end;

procedure TWin32Mouse.SetConsoleSize(AConsoleWidth, AConsoleHeight: Integer);
begin
  FConsoleWidth := AConsoleWidth;
  FConsoleHeight := AConsoleHeight;
end;

procedure TWin32Mouse.Enable;
begin
  { enable buffering }
  FEnabled := True;
end;

procedure TWin32Mouse.Disable;
begin
  { disable buffering }
  FEnabled := False;
end;

function TWin32Mouse.MoveMouseTo(X, Y: Integer): Boolean;
var
  WindowRect: RECT;
  pt: TPOINT;
begin
  if (X < 0) or (X >= FConsoleWidth) or (Y <= 0) or (Y >= FConsoleHeight) then
    exit(False);

  if not FFullScreen then
  begin
    if not GetClientRect(FWindow, WindowRect) then
      exit(False);

    FWindowX1 := WindowRect.left;
    FWindowY1 := WindowRect.top;
    FWindowX2 := WindowRect.right - 1;
    FWindowY2 := WindowRect.bottom - 1;
  end;

  pt.X := X * (FWindowX2 - FWindowX1) div (FConsoleWidth - 1) + FWindowX1;
  pt.Y := Y * (FWindowY2 - FWindowY1) div (FConsoleHeight - 1) + FWindowY1;

  if not ClientToScreen(FWindow, pt) then
    exit(False);

  Result := SetCursorPos(pt.X, pt.Y);
end;

function TWin32Mouse.WndProc(hWnd: HWND; message: DWord; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  fwKeys: Integer;
  CurPos: POINT;
  LButton, MButton, RButton, XButton1, XButton2: Boolean;
  TranslatedXPos, TranslatedYPos: Integer;
  PTCMouseButtonState: TPTCMouseButtonState;
  WindowRect: RECT;
  ScrollAmount: Integer;

  button: TPTCMouseButton;
  before, after: Boolean;
  cstate: TPTCMouseButtonState;
  I: Integer;
begin
  Result := 0;
  { check enabled flag }
  if not FEnabled then
    exit;

  if (message = WM_MOUSEMOVE) or
     (message = WM_LBUTTONDOWN) or (message = WM_LBUTTONUP) or (message = WM_LBUTTONDBLCLK) or
     (message = WM_MBUTTONDOWN) or (message = WM_MBUTTONUP) or (message = WM_MBUTTONDBLCLK) or
     (message = WM_RBUTTONDOWN) or (message = WM_RBUTTONUP) or (message = WM_RBUTTONDBLCLK) or
     (message = WM_XBUTTONDOWN) or (message = WM_XBUTTONUP) or (message = WM_XBUTTONDBLCLK) or
     (message = WM_MOUSEWHEEL) or (message = WM_MOUSEHWHEEL) then
  begin
    fwKeys := Word(wParam); {MK_LBUTTON or MK_MBUTTON or MK_RBUTTON or MK_CONTROL or MK_SHIFT}
    CurPos.x := GET_X_LPARAM(lParam);
    CurPos.y := GET_Y_LPARAM(lParam);

    if (message = WM_MOUSEWHEEL) or (message = WM_MOUSEHWHEEL) then
    begin
      ScrollAmount := SmallInt(wParam shr 16);
      { for WM_MOUSEWHEEL and WM_MOUSEHWHEEL, windows returns cursor position in
        screen coordinates, instead of client coordinates, so convert them }
      ScreenToClient(hWnd, CurPos);
    end;

    LButton := (fwKeys and MK_LBUTTON) <> 0;
    MButton := (fwKeys and MK_MBUTTON) <> 0;
    RButton := (fwKeys and MK_RBUTTON) <> 0;
    XButton1 := (fwKeys and MK_XBUTTON1) <> 0;
    XButton2 := (fwKeys and MK_XBUTTON2) <> 0;

    if not FFullScreen then
    begin
      GetClientRect(hWnd, WindowRect);

      FWindowX1 := WindowRect.left;
      FWindowY1 := WindowRect.top;
      FWindowX2 := WindowRect.right - 1;
      FWindowY2 := WindowRect.bottom - 1;
    end;

    if (CurPos.x >= FWindowX1) and (CurPos.y >= FWindowY1) and
       (CurPos.x <= FWindowX2) and (CurPos.y <= FWindowY2) then
    begin
      if FWindowX2 <> FWindowX1 then
        TranslatedXPos := (CurPos.x - FWindowX1) * (FConsoleWidth  - 1) div (FWindowX2 - FWindowX1)
      else { avoid div by zero }
        TranslatedXPos := 0;

      if FWindowY2 <> FWindowY1 then
        TranslatedYPos := (CurPos.y - FWindowY1) * (FConsoleHeight - 1) div (FWindowY2 - FWindowY1)
      else { avoid div by zero }
        TranslatedYPos := 0;

      { Just in case... }
      if TranslatedXPos < 0 then
        TranslatedXPos := 0;
      if TranslatedYPos < 0 then
        TranslatedYPos := 0;
      if TranslatedXPos >= FConsoleWidth then
        TranslatedXPos := FConsoleWidth - 1;
      if TranslatedYPos >= FConsoleHeight then
        TranslatedYPos := FConsoleHeight - 1;

      if not LButton then
        PTCMouseButtonState := []
      else
        PTCMouseButtonState := [PTCMouseButton1];
      if RButton then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton2];
      if MButton then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton3];
      if XButton1 then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton8];
      if XButton2 then
        PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton9];

      if not FPreviousMousePositionSaved then
      begin
        FPreviousMouseX := TranslatedXPos; { first DeltaX will be 0 }
        FPreviousMouseY := TranslatedYPos; { first DeltaY will be 0 }
        FPreviousMouseButtonState := [];
      end;

      { movement? }
      if (TranslatedXPos <> FPreviousMouseX) or (TranslatedYPos <> FPreviousMouseY) then
        FEventQueue.AddEvent(TPTCMouseEvent.Create(TranslatedXPos, TranslatedYPos, TranslatedXPos - FPreviousMouseX, TranslatedYPos - FPreviousMouseY, FPreviousMouseButtonState));

      { button presses/releases? }
      cstate := FPreviousMouseButtonState;
      for button := Low(button) to High(button) do
      begin
        before := button In FPreviousMouseButtonState;
        after := button In PTCMouseButtonState;
        if after and (not before) then
        begin
          { button was pressed }
          cstate := cstate + [button];
          FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, True, button));
        end
        else
          if before and (not after) then
          begin
            { button was released }
            cstate := cstate - [button];
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, button));
          end;
      end;

      { scroll wheel? }
      if (message = WM_MOUSEWHEEL) and (Abs(ScrollAmount) >= WHEEL_DELTA) then
        if ScrollAmount > 0 then
          for I := 1 to ScrollAmount div WHEEL_DELTA do
          begin
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate + [PTCMouseButton4], True, PTCMouseButton4));
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, PTCMouseButton4));
          end
        else
          for I := 1 to Abs(ScrollAmount) div WHEEL_DELTA do
          begin
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate + [PTCMouseButton5], True, PTCMouseButton5));
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, PTCMouseButton5));
          end;
      if (message = WM_MOUSEHWHEEL) and (Abs(ScrollAmount) >= WHEEL_DELTA) then
        if ScrollAmount > 0 then
          for I := 1 to ScrollAmount div WHEEL_DELTA do
          begin
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate + [PTCMouseButton7], True, PTCMouseButton7));
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, PTCMouseButton7));
          end
        else
          for I := 1 to Abs(ScrollAmount) div WHEEL_DELTA do
          begin
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate + [PTCMouseButton6], True, PTCMouseButton6));
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(TranslatedXPos, TranslatedYPos, 0, 0, cstate, False, PTCMouseButton6));
          end;

      FPreviousMouseX := TranslatedXPos;
      FPreviousMouseY := TranslatedYPos;
      FPreviousMouseButtonState := PTCMouseButtonState;
      FPreviousMousePositionSaved := True;
    end;
  end;
end;
