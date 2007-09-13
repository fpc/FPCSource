{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

{ $R win32\base\ptcres.res}

{ bug in the compiler???}
{ $LINKLIB ptc.owr}

Constructor TWin32Window.Create(window : HWND);

Begin
  LOG('attaching to user managed window');
  defaults;
  m_window := window;
  m_managed := False;
End;

Constructor TWin32Window.Create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean; data : Pointer);

Begin
  internal_create(wndclass, title, extra, style, show, x, y, width, height, center, _multithreaded, data);
End;

Constructor TWin32Window.Create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean);

Begin
  internal_create(wndclass, title, extra, style, show, x, y, width, height, center, _multithreaded, Nil);
End;

Destructor TWin32Window.Destroy;

Begin
  close;
  Inherited Destroy;
End;

Procedure TWin32Window.cursor(flag : Boolean);

Begin
  If flag Then
  Begin
    SetClassLong(m_window, GCL_HCURSOR, LoadCursor(0, IDC_ARROW));
  End
  Else
  Begin
    SetClassLong(m_window, GCL_HCURSOR, 0);
  End;
  SendMessage(m_window, WM_SETCURSOR, 0, 0);
End;

Procedure TWin32Window.resize(width, height : Integer);

Var
  window_rectangle : RECT;
  rectangle : RECT;

Begin
  GetWindowRect(m_window, window_rectangle);
  With rectangle Do
  Begin
    left := 0;
    top := 0;
    right := width;
    bottom := height;
  End;
  AdjustWindowRectEx(rectangle, m_style, False, m_extra);
  SetWindowPos(m_window, HWND_TOP, window_rectangle.left,
      window_rectangle.top, rectangle.right - rectangle.left,
      rectangle.bottom - rectangle.top, 0);
  {
  todo: detect if the window is resized off the screen and let windows reposition it correctly... ?
  }
End;

Procedure TWin32Window.update(force : Boolean);

Var
  message : MSG;

Begin
  If (Not m_managed) And (Not force) Then
    Exit;
  If Not m_multithreaded Then
  Begin
    While PeekMessage(message, m_window, 0, 0, PM_REMOVE) Do
    Begin
      TranslateMessage(message);
      DispatchMessage(message);
    End;
  End
  Else
    Sleep(0);
End;

Procedure TWin32Window.update; {force = False}

Begin
  update(False);
End;

Function TWin32Window.handle : HWND;

Begin
  handle := m_window;
End;

Function TWin32Window.thread : DWord;

Begin
  If m_multithreaded Then
    thread := m_id
  Else
    thread := GetCurrentThreadId;
End;

Function TWin32Window.managed : Boolean;

Begin
  managed := m_managed;
End;

Function TWin32Window.multithreaded : Boolean;

Begin
  multithreaded := m_multithreaded;
End;

Function WndProcSingleThreaded(hWnd : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; StdCall;

Begin
  Case message Of
    WM_CLOSE : Begin
      LOG('TWin32Window WM_CLOSE');
      Halt(0);
    End;
    Else
      WndProcSingleThreaded := DefWindowProc(hWnd, message, wParam, lParam);
  End;
End;

Function WndProcMultiThreaded(hWnd : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; StdCall;

Begin
  WndProcMultiThreaded := 0;
  Case message Of
    WM_DESTROY : Begin
      LOG('TWin32Window WM_DESTROY');
      PostQuitMessage(0);
    End;
    WM_CLOSE : Begin
      LOG('TWin32Window WM_CLOSE');
      Halt(0);
    End;
    Else
      WndProcMultiThreaded := DefWindowProc(hWnd, message, wParam, lParam);
  End;
End;

Procedure TWin32Window.internal_create(wndclass, title : String; extra, style : DWord; show, x, y, width, height : Integer; center, _multithreaded : Boolean; data : Pointer);

Var
  program_instance{, library_instance} : DWord;
  rectangle : RECT;
  display_width, display_height : Integer;
  wc : WNDCLASSEX;

Begin
  LOG('creating managed window');
  defaults;
  m_multithreaded := _multithreaded;
  wndclass := wndclass + #0;
  title := title + #0;
  Try
    program_instance := GetModuleHandle(Nil);
{    library_instance := program_instance;}
    wc.cbSize := SizeOf(WNDCLASSEX);
    wc.hInstance := program_instance;
    wc.lpszClassName := @wndclass[1];
    wc.style := CS_VREDRAW Or CS_HREDRAW;
    wc.hIcon := 0{LoadIcon(library_instance, 'IDI_PTC_ICON')};
    wc.hIconSm := 0;
    wc.lpszMenuName := Nil;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;
    wc.hbrBackground := 0;{(HBRUSH) GetStockObject(BLACK_BRUSH)}
    If multithreaded Then
      wc.lpfnWndProc := @WndProcMultiThreaded
    Else
      wc.lpfnWndProc := @WndProcSingleThreaded;
    wc.hCursor := LoadCursor(0, IDC_ARROW);
    RegisterClassEx(wc);
    With rectangle Do
    Begin
      left := 0;
      top := 0;
      right := width;
      bottom := height;
    End;
    AdjustWindowRectEx(rectangle, style, False, extra);
    If center Then
    Begin
      LOG('centering window');
      display_width := GetSystemMetrics(SM_CXSCREEN);
      display_height := GetSystemMetrics(SM_CYSCREEN);
      x := (display_width - (rectangle.right - rectangle.left)) Div 2;
      y := (display_height - (rectangle.bottom - rectangle.top)) Div 2;
    End;
    m_name := wndclass;
    m_title := title;
    m_extra := extra;
    m_style := style;
    m_show := show;
    m_x := x;
    m_y := y;
    m_width := rectangle.right - rectangle.left;
    m_height := rectangle.bottom - rectangle.top;
    m_data := data;
    If multithreaded Then
    Begin
      {...}
    End
    Else
    Begin
      m_window := CreateWindowEx(m_extra, PChar(m_name), PChar(m_title), m_style, m_x, m_y, m_width, m_height, 0, 0, 0, m_data);
      If Not IsWindow(m_window) Then
        Raise TPTCError.Create('could not create window');
      ShowWindow(m_window, m_show);
      SetFocus(m_window);
      SetActiveWindow(m_window);
      SetForegroundWindow(m_window);
    End;
  Except
    On error : TPTCError Do
      Raise TPTCError.Create('could not create window', error);
  End;
End;

Procedure TWin32Window.defaults;

Begin
  m_window := 0;
  m_event := 0;
  m_thread := 0;
  m_id := 0;
  m_name := '';
  m_title := '';
  m_extra := 0;
  m_style := 0;
  m_show := 0;
  m_x := 0;
  m_y := 0;
  m_width := 0;
  m_height := 0;
  m_data := Nil;
  m_managed := True;
  m_multithreaded := False;
End;

Procedure TWin32Window.close;

Begin
  If Not m_managed Then
  Begin
    LOG('detaching from user managed window');
    m_window := 0;
  End
  Else
  Begin
    LOG('closing managed window');
    If m_multithreaded Then
    Begin
      If (m_thread <> 0) And IsWindow(m_window) Then
      Begin
        PostMessage(m_window, WM_DESTROY, 0, 0);
        WaitForSingleObject(m_thread, INFINITE);
      End;
      If m_event <> 0 Then
        CloseHandle(m_event);
      If m_thread <> 0 Then
        CloseHandle(m_thread);
      SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
    End
    Else
      If (m_window <> 0) And IsWindow(m_window) Then
        DestroyWindow(m_window);
    m_window := 0;
    m_event := 0;
    m_thread := 0;
    m_id := 0;
    UnregisterClass(PChar(m_name), GetModuleHandle(Nil));
  End;
End;

Class Procedure TWin32Window.ThreadFunction(owner : TWin32Window);

Var
  message : MSG;

Begin
  With owner Do
  Begin
    m_window := CreateWindowEx(m_extra, PChar(m_name), PChar(m_title), m_style, m_x, m_y, m_width, m_height, 0, 0, 0, m_data);
    If IsWindow(m_window) Then
    Begin
      ShowWindow(m_window, m_show);
      SetFocus(m_window);
      SetForegroundWindow(m_window);
      SetEvent(m_event);
      While GetMessage(message, 0, 0, 0) = True Do
      Begin
        TranslateMessage(message);
        DispatchMessage(message);
      End;
    End
    Else
      SetEvent(m_event);
  End;
End;
