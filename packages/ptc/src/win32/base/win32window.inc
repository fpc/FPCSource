{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2012  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef VER2_6}
type
  WNDCLASSEXA = WNDCLASSEX;
{$endif VER2_6}

{ $R win32\base\ptcres.res}

{ bug in the compiler???}
{ $LINKLIB ptc.owr}

constructor TWin32Window.Create(window: HWND);
begin
  LOG('attaching to user managed window');
  defaults;
  FWindow := window;
  FManaged := False;
end;

function WndProcSingleThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; forward;
function WndProcMultiThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; forward;

constructor TWin32Window.Create(const AWndClass, ATitle: string; AExtra, AStyle, AClassStyle: DWord;
                                AShow, AX, AY, AWidth, AHeight: Integer; ACenter, AMultithreaded,
                                ACursor, AInterceptClose: Boolean);
var
  program_instance{, library_instance}: DWord;
  rectangle: RECT;
  display_width, display_height: Integer;
  wc: WNDCLASSEXA;
begin
  LOG('creating managed window');
  Defaults;
  FMultithreaded := AMultithreaded;
  try
    FInterceptClose := AInterceptClose;
    program_instance := GetModuleHandle(nil);
{    library_instance := program_instance;}
    wc.cbSize := SizeOf(wc);
    wc.hInstance := program_instance;
    wc.lpszClassName := PChar(AWndClass);
    wc.style := AClassStyle;
    wc.hIcon := 0{LoadIcon(library_instance, 'IDI_PTC_ICON')};
    wc.hIconSm := 0;
    wc.lpszMenuName := nil;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;
    wc.hbrBackground := 0;{(HBRUSH) GetStockObject(BLACK_BRUSH)}
    if AMultithreaded then
      wc.lpfnWndProc := @WndProcMultiThreaded
    else
      wc.lpfnWndProc := @WndProcSingleThreaded;
    if ACursor then
      wc.hCursor := LoadCursor(0, IDC_ARROW)
    else
      wc.hCursor := 0;
    RegisterClassExA(wc);
    with rectangle do
    begin
      left := 0;
      top := 0;
      right := AWidth;
      bottom := AHeight;
    end;
    AdjustWindowRectEx(rectangle, AStyle, False, AExtra);
    if ACenter then
    begin
      LOG('centering window');
      display_width := GetSystemMetrics(SM_CXSCREEN);
      display_height := GetSystemMetrics(SM_CYSCREEN);
      AX := (display_width - (rectangle.right - rectangle.left)) div 2;
      AY := (display_height - (rectangle.bottom - rectangle.top)) div 2;
    end;
    FName := AWndClass;
    FTitle := ATitle;
    FExtra := AExtra;
    FStyle := AStyle;
    FShow := AShow;
    FX := AX;
    FY := AY;
    FWidth := rectangle.right - rectangle.left;
    FHeight := rectangle.bottom - rectangle.top;
    if AMultithreaded then
    begin
      {...}
    end
    else
    begin
      FWindow := CreateWindowExA(FExtra, PChar(FName), PChar(FTitle), FStyle, FX, FY, FWidth, FHeight, 0, 0, 0, Self);
      if not IsWindow(FWindow) then
        raise TPTCError.Create('could not create window');
      ShowWindow(FWindow, FShow);
      SetFocus(FWindow);
      SetActiveWindow(FWindow);
      SetForegroundWindow(FWindow);
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('could not create window', error);
  end;
end;

destructor TWin32Window.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TWin32Window.Cursor(AFlag: Boolean);
begin
  if AFlag then
  begin
//    SetClassLong(FWindow, GCL_HCURSOR, LoadCursor(0, IDC_ARROW));
    SetClassLongPtr(FWindow, GCLP_HCURSOR, LoadCursor(0, IDC_ARROW));
  end
  else
  begin
//    SetClassLong(FWindow, GCL_HCURSOR, 0);
    SetClassLongPtr(FWindow, GCLP_HCURSOR, 0);
  end;
  SendMessage(FWindow, WM_SETCURSOR, 0, 0);
end;

procedure TWin32Window.ConfineCursor(AFlag: Boolean);
var
  rct: TRECT;
  p1, p2: TPOINT;
begin
  if AFlag then
  begin
    GetClientRect(FWindow, @rct);
    p1.x := rct.left;
    p1.y := rct.top;
    p2.x := rct.right;
    p2.y := rct.bottom;
    ClientToScreen(FWindow, @p1);
    ClientToScreen(FWindow, @p2);
    rct.left := p1.x;
    rct.top := p1.y;
    rct.right := p2.x;
    rct.bottom := p2.y;
    ClipCursor(@rct);
    FCursorConfineInEffect := True;
  end
  else
  begin
    if FCursorConfineInEffect then
    begin
      ClipCursor(nil);
      FCursorConfineInEffect := False;
    end;
  end;
end;

procedure TWin32Window.Resize(AWidth, AHeight: Integer);
var
  window_rectangle: RECT;
  rectangle: RECT;
begin
  GetWindowRect(FWindow, window_rectangle);
  with rectangle do
  begin
    left := 0;
    top := 0;
    right := AWidth;
    bottom := AHeight;
  end;
  AdjustWindowRectEx(rectangle, FStyle, False, FExtra);
  SetWindowPos(FWindow, HWND_TOP, window_rectangle.left,
      window_rectangle.top, rectangle.right - rectangle.left,
      rectangle.bottom - rectangle.top, 0);
  {
  todo: detect if the window is resized off the screen and let windows reposition it correctly... ?
  }
end;

procedure TWin32Window.Update(AForce: Boolean = False; AWaitForMessage: Boolean = False);
var
  message: MSG;
begin
  if (not FManaged) and (not AForce) then
    exit;
  if not FMultithreaded then
  begin
    { updated to pump all window messages, and not just for our FWindow;
      this fixes keyboard layout switching and maybe other bugs and side effects...
      Seems like Windows wants everything pumped :) }
    if AWaitForMessage then
    begin
      GetMessage(message, {FWindow}0, 0, 0);
      TranslateMessage(message);
      DispatchMessage(message);
    end
    else
      while PeekMessage(message, {FWindow}0, 0, 0, PM_REMOVE) do
      begin
        TranslateMessage(message);
        DispatchMessage(message);
      end;
  end
  else
    Sleep(0);
end;

function TWin32Window.GetThread: DWord;
begin
  if FMultithreaded then
    Result := FID
  else
    Result := GetCurrentThreadId;
end;

function WndProcSingleThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WindowObject: TWin32Window;
  pCreate: PCREATESTRUCT;
begin
  case message of
    WM_CREATE:
      begin
        pCreate := PCREATESTRUCT(lParam);
        WindowObject := TWin32Window(pCreate^.lpCreateParams);
        SetWindowLongPtr(hWnd, GWLP_USERDATA, LONG_PTR(WindowObject));
        Result := WindowObject.WMCreate(hWnd, message, wParam, lParam);
      end;
    WM_DESTROY:
      begin
        WindowObject := TWin32Window(GetWindowLongPtr(hWnd, GWLP_USERDATA));
        Result := WindowObject.WMDestroy(hWnd, message, wParam, lParam);
      end;
    WM_SYSCOMMAND:
      begin
        { this fixes the pausing of the application when the Alt or F10 key is pressed }
        if wParam = SC_KEYMENU then
	  Result := 0
	else
          Result := DefWindowProcA(hWnd, message, wParam, lParam);
      end;
    WM_SETCURSOR: begin
      if (LOWORD(lParam) = HTCLIENT) and (GetClassLongPtr(hWnd, GCLP_HCURSOR) = 0) then
      begin
        SetCursor(0);
	Result := 1;
      end
      else
        Result := DefWindowProcA(hWnd, message, wParam, lParam);
    end;
    WM_CLOSE: begin
      LOG('TWin32Window WM_CLOSE');
      WindowObject := TWin32Window(GetWindowLongPtr(hWnd, GWLP_USERDATA));
      if WindowObject.InterceptClose then
        Result := 0
      else
        Halt(0);
    end;
    else
      Result := DefWindowProcA(hWnd, message, wParam, lParam);
  end;
end;

function WndProcMultiThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := 0;
  case message of
    WM_SYSCOMMAND:
      begin
        { this fixes the pausing of the application when the Alt or F10 key is pressed }
        if wParam = SC_KEYMENU then
	  Result := 0
	else
          Result := DefWindowProcA(hWnd, message, wParam, lParam);
      end;
    WM_SETCURSOR: begin
      if (LOWORD(lParam) = HTCLIENT) and (GetClassLongPtr(hWnd, GCLP_HCURSOR) = 0) then
      begin
        SetCursor(0);
	Result := 1;
      end
      else
        Result := DefWindowProcA(hWnd, message, wParam, lParam);
    end;
    WM_DESTROY: begin
      LOG('TWin32Window WM_DESTROY');
      PostQuitMessage(0);
    end;
    WM_CLOSE: begin
      LOG('TWin32Window WM_CLOSE');
      Halt(0);
    end;
    else
      Result := DefWindowProcA(hWnd, message, wParam, lParam);
  end;
end;

procedure TWin32Window.Defaults;
begin
  FWindow := 0;
  FEvent := 0;
  FThread := 0;
  FID := 0;
  FName := '';
  FTitle := '';
  FExtra := 0;
  FStyle := 0;
  FShow := 0;
  FX := 0;
  FY := 0;
  FWidth := 0;
  FHeight := 0;
  FManaged := True;
  FMultithreaded := False;
  FInterceptClose := False;
end;

procedure TWin32Window.Close;
begin
  if FCursorConfineInEffect then
    ConfineCursor(False);

  if not FManaged then
  begin
    LOG('detaching from user managed window');
    FWindow := 0;
  end
  else
  begin
    LOG('closing managed window');
    if FMultithreaded then
    begin
      if (FThread <> 0) and IsWindow(FWindow) then
      begin
        PostMessage(FWindow, WM_DESTROY, 0, 0);
        WaitForSingleObject(FThread, INFINITE);
      end;
      if FEvent <> 0 then
        CloseHandle(FEvent);
      if FThread <> 0 then
        CloseHandle(FThread);
      SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
    end
    else
      if (FWindow <> 0) and IsWindow(FWindow) then
        DestroyWindow(FWindow);
    FWindow := 0;
    FEvent := 0;
    FThread := 0;
    FID := 0;
    UnregisterClassA(PChar(FName), GetModuleHandle(Nil));
  end;
end;

class procedure TWin32Window.ThreadFunction(AOwner: TWin32Window);
var
  message: MSG;
begin
  with AOwner do
  begin
    FWindow := CreateWindowEx(FExtra, PChar(FName), PChar(FTitle), FStyle, FX, FY, FWidth, FHeight, 0, 0, 0, nil);
    if IsWindow(FWindow) then
    begin
      ShowWindow(FWindow, FShow);
      SetFocus(FWindow);
      SetForegroundWindow(FWindow);
      SetEvent(FEvent);
      while GetMessage(message, 0, 0, 0) = True do
      begin
        TranslateMessage(message);
        DispatchMessage(message);
      end;
    end
    else
      SetEvent(FEvent);
  end;
end;

function TWin32Window.WMCreate(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result := DefWindowProcA(hWnd, uMsg, wParam, lParam);
end;

function TWin32Window.WMDestroy(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result := DefWindowProcA(hWnd, uMsg, wParam, lParam);
end;
