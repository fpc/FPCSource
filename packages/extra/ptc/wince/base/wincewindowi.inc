Type
  PWndProcRegEntry = ^TWndProcRegEntry;
  TWndProcRegEntry = Record
    WindowHandle : HWND;
    Handler : TWinCEWndProc;
  End;

ThreadVar
  WndProcRegistry : Array Of TWndProcRegEntry;
  WndProcRegistryCache : Integer;

Procedure WndProcAdd(AWindowHandle : HWND; AHandler : TWinCEWndProc);

Var
  I : Integer;

Begin
  I := Length(WndProcRegistry);
  SetLength(WndProcRegistry, I + 1);
  WndProcRegistry[I].WindowHandle := AWindowHandle;
  WndProcRegistry[I].Handler := AHandler;
End;

Procedure WndProcRemove(AWindowHandle : HWND);

Var
  I, J : Integer;

Begin
  J := 0;
  For I := Low(WndProcRegistry) To High(WndProcRegistry) Do
    If WndProcRegistry[I].WindowHandle <> AWindowHandle Then
    Begin
      WndProcRegistry[J] := WndProcRegistry[I];
      Inc(J);
    End;
  SetLength(WndProcRegistry, J);
End;

Function WndProcFind(AWindowHandle : HWND) : TWinCEWndProc;

Var
  I : Integer;

Begin
  If (WndProcRegistryCache >= Low(WndProcRegistry)) And
     (WndProcRegistryCache <= High(WndProcRegistry)) And
     (WndProcRegistry[WndProcRegistryCache].WindowHandle = AWindowHandle) Then
  Begin
    Result := WndProcRegistry[WndProcRegistryCache].Handler;
    Exit;
  End;

  For I := Low(WndProcRegistry) To High(WndProcRegistry) Do
    If WndProcRegistry[I].WindowHandle = AWindowHandle Then
    Begin
      Result := WndProcRegistry[I].Handler;
      WndProcRegistryCache := I;
      Exit;
    End;
  Result := Nil;
End;

Function WinCEWindowProc(Ahwnd : HWND; AuMsg : UINT; AwParam : WPARAM; AlParam : LPARAM) : LRESULT; CDecl;

Var
  Handler : TWinCEWndProc;

Begin
  Handler := WndProcFind(Ahwnd);
  If Handler <> Nil Then
    Result := Handler(Ahwnd, AuMsg, AwParam, AlParam)
  Else
    Result := DefWindowProcW(Ahwnd, AuMsg, AwParam, AlParam);
End;

Constructor TWinCEWindow.Create(Const AClassName, ATitle : WideString;
                                AExStyle, AStyle : DWord;
				AShow, AX, AY, AWidth, AHeight : Integer;
                                AWndProc : TWinCEWndProc;
				AData : Pointer = Nil);

Var
  ClassAtom : ATOM;
  wc : WNDCLASSW;
  ProgramInstance : HANDLE;
  Rectangle : RECT;
  X, Y, Width, Height : Integer;

Begin
  ProgramInstance := GetModuleHandleW(Nil);
  If ProgramInstance = 0 Then
    Raise TPTCError.Create('could not get module handle');

  LOG('registering window class');
  FillChar(wc, SizeOf(wc), 0);
  wc.style := CS_DBLCLKS{ Or CS_HREDRAW Or CS_VREDRAW};
  wc.lpfnWndProc := @WinCEWindowProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  wc.hInstance := ProgramInstance;
  wc.hIcon := 0; { not supported by WinCE }
  wc.hCursor := 0;
  wc.hbrBackground := 0;
  wc.lpszMenuName := Nil;
  wc.lpszClassName := PWideChar(AClassName);
  ClassAtom := RegisterClassW(@wc);
  If ClassAtom = 0 Then
    Raise TPTCError.Create('could not register window class');
  FClassName := AClassName;
  FClassHInstance := wc.hInstance;

  With Rectangle Do
  Begin
    left := 0;
    top := 0;
    right := AWidth;
    bottom := AHeight;
  End;
  If Not AdjustWindowRectEx(@Rectangle, AStyle, False, AExStyle) Then
    Raise TPTCError.Create('could not AdjustWindowRectEx');

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
  If (FWindow = 0) Or Not IsWindow(FWindow) Then
    Raise TPTCError.Create('could not create window');
  LOG('installing window message handler');
  WndProcAdd(FWindow, AWndProc);
  ShowWindow(FWindow, AShow);
  If SetFocus(FWindow) = 0 Then
    Raise TPTCError.Create('could not set focus to the new window');
  If SetActiveWindow(FWindow) = 0 Then
    Raise TPTCError.Create('could not set active window');
  If Not SetForegroundWindow(FWindow) Then
    Raise TPTCError.Create('could not set foreground window');
  {...}
End;

Destructor TWinCEWindow.Destroy;

Begin
  Close;
  Inherited Destroy;
End;

Procedure TWinCEWindow.Close;

Begin
  If (FWindow <> 0) And IsWindow(FWindow) Then
  Begin
    WndProcRemove(FWindow);
    DestroyWindow(FWindow);
  End;
  FWindow := 0;

  If FClassName <> '' Then
    UnregisterClass(PWideChar(FClassName), FClassHInstance);
  FClassName := '';
End;

Procedure TWinCEWindow.Update;

Var
  Message : MSG;

Begin
  While PeekMessage(@Message, FWindow, 0, 0, PM_REMOVE) Do
  Begin
    TranslateMessage(@Message);
    DispatchMessage(@Message);
  End;
End;
