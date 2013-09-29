{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2007, 2009, 2010  Nikolay Nikolov (nickysn@users.sourceforge.net)

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  PWndProcRegEntry = ^TWndProcRegEntry;
  TWndProcRegEntry = record
    WindowHandle: HWND;
    Handler: TWinCEWndProc;
  end;

ThreadVar
  WndProcRegistry: array of TWndProcRegEntry;
  WndProcRegistryCache: Integer;

procedure WndProcAdd(AWindowHandle: HWND; AHandler: TWinCEWndProc);

var
  I: Integer;

begin
  I := Length(WndProcRegistry);
  SetLength(WndProcRegistry, I + 1);
  WndProcRegistry[I].WindowHandle := AWindowHandle;
  WndProcRegistry[I].Handler := AHandler;
end;

procedure WndProcRemove(AWindowHandle: HWND);

var
  I, J: Integer;

begin
  J := 0;
  for I := Low(WndProcRegistry) to High(WndProcRegistry) do
    if WndProcRegistry[I].WindowHandle <> AWindowHandle then
    begin
      WndProcRegistry[J] := WndProcRegistry[I];
      Inc(J);
    end;
  SetLength(WndProcRegistry, J);
end;

function WndProcFind(AWindowHandle: HWND): TWinCEWndProc;

var
  I: Integer;

begin
  if (WndProcRegistryCache >= Low(WndProcRegistry)) and
     (WndProcRegistryCache <= High(WndProcRegistry)) and
     (WndProcRegistry[WndProcRegistryCache].WindowHandle = AWindowHandle) then
  begin
    Result := WndProcRegistry[WndProcRegistryCache].Handler;
    exit;
  end;

  for I := Low(WndProcRegistry) to High(WndProcRegistry) do
    if WndProcRegistry[I].WindowHandle = AWindowHandle then
    begin
      Result := WndProcRegistry[I].Handler;
      WndProcRegistryCache := I;
      exit;
    end;
  Result := nil;
end;

function WinCEWindowProc(Ahwnd: HWND; AuMsg: UINT; AwParam: WPARAM; AlParam: LPARAM): LRESULT; cdecl;

var
  Handler: TWinCEWndProc;

begin
  Handler := WndProcFind(Ahwnd);
  if Handler <> nil then
    Result := Handler(Ahwnd, AuMsg, AwParam, AlParam)
  else
    Result := DefWindowProcW(Ahwnd, AuMsg, AwParam, AlParam);
end;

constructor TWinCEWindow.Create(const AClassName, ATitle: WideString;
                                AExStyle, AStyle: DWord;
				AShow, AX, AY, AWidth, AHeight: Integer;
                                AWndProc: TWinCEWndProc;
				AData: Pointer = Nil);

var
  ClassAtom: ATOM;
  wc: WNDCLASSW;
  ProgramInstance: HANDLE;
  Rectangle: RECT;
  X, Y, Width, Height: Integer;

begin
  ProgramInstance := GetModuleHandleW(Nil);
  if ProgramInstance = 0 then
    raise TPTCError.Create('could not get module handle');

  LOG('registering window class');
  FillChar(wc, SizeOf(wc), 0);
  wc.style := CS_DBLCLKS{ or CS_HREDRAW or CS_VREDRAW};
  wc.lpfnWndProc := @WinCEWindowProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  wc.hInstance := ProgramInstance;
  wc.hIcon := 0; { not supported by WinCE }
  wc.hCursor := 0;
  wc.hbrBackground := 0;
  wc.lpszMenuName := nil;
  wc.lpszClassName := PWideChar(AClassName);
  ClassAtom := RegisterClassW(@wc);
  if ClassAtom = 0 then
    raise TPTCError.Create('could not register window class');
  FClassName := AClassName;
  FClassHInstance := wc.hInstance;

  With Rectangle do
  begin
    left := 0;
    top := 0;
    right := AWidth;
    bottom := AHeight;
  end;
  if not AdjustWindowRectEx(@Rectangle, AStyle, False, AExStyle) then
    raise TPTCError.Create('could not AdjustWindowRectEx');

  X := AX;
  Y := AY;
  Width := Rectangle.right - Rectangle.left;
  Height := Rectangle.bottom - Rectangle.top;

  FWindow := CreateWindowExW(AExStyle,
                             PWideChar(AClassName),
			     PWideChar(ATitle),
			     AStyle,
			     X, Y, Width, Height,
			     0, 0, 0,
			     AData);
  if (FWindow = 0) or not IsWindow(FWindow) then
    raise TPTCError.Create('could not create window');
  LOG('installing window message handler');
  WndProcAdd(FWindow, AWndProc);
  ShowWindow(FWindow, AShow);
  if SetFocus(FWindow) = 0 then
    raise TPTCError.Create('could not set focus to the new window');
  if SetActiveWindow(FWindow) = 0 then
    raise TPTCError.Create('could not set active window');
  if not SetForegroundWindow(FWindow) then
    raise TPTCError.Create('could not set foreground window');
  {...}
end;

destructor TWinCEWindow.Destroy;

begin
  Close;
  inherited Destroy;
end;

procedure TWinCEWindow.Close;

begin
  if (FWindow <> 0) and IsWindow(FWindow) then
  begin
    WndProcRemove(FWindow);
    DestroyWindow(FWindow);
  end;
  FWindow := 0;

  if FClassName <> '' then
    UnregisterClass(PWideChar(FClassName), FClassHInstance);
  FClassName := '';
end;

procedure TWinCEWindow.Update;

var
  Message: MSG;

begin
  while PeekMessage(@Message, FWindow, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(@Message);
    DispatchMessage(@Message);
  end;
end;
