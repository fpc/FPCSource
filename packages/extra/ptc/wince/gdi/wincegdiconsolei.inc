Constructor TWinCEGDIConsole.Create;

Begin
  Inherited Create;

  FDefaultWidth := 320;
  FDefaultHeight := 200;
  FDefaultFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;
End;

Destructor TWinCEGDIConsole.Destroy;

Begin
  Close;

  FWindow.Free;

  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;
  FArea.Free;
  FClip.Free;
  FDefaultFormat.Free;

  Inherited Destroy;
End;

Procedure TWinCEGDIConsole.Open(Const ATitle : String; APages : Integer = 0);

Begin
  Open(ATitle, FDefaultFormat, APages);
End;

Procedure TWinCEGDIConsole.Open(Const ATitle : String; Const AFormat : TPTCFormat;
                                APages : Integer = 0);

Begin
  Open(ATitle, FDefaultWidth, FDefaultHeight, AFormat, APages);
End;

Procedure TWinCEGDIConsole.Open(Const ATitle : String; Const AMode : TPTCMode;
                                APages : Integer = 0);

Begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
End;

Procedure TWinCEGDIConsole.Open(Const ATitle : String; AWidth, AHeight : Integer;
                                Const AFormat : TPTCFormat; APages : Integer = 0);

Var
  DeviceContext : HDC;
  tmp : TPTCArea;

Begin
  LOG('TWinCEGDIConsole.Open');

  If FBitmap <> 0 Then
  Begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  End;
  FreeAndNil(FWindow);
  FreeAndNil(FBitmapInfo);
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  LOG('creating window');
  FWindow := TWinCEWindow.Create('PTC_GDI_WINDOWED_FIXED',
                                 ATitle,
                                 0,
                                 WS_VISIBLE {Or WS_SYSMENU Or WS_CAPTION},
                                 SW_SHOWNORMAL,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 AWidth, AHeight,
				 @WndProc);
  LOG('window created successfully');

  FBitmapInfo := TWinCEBitmapInfo.Create(AWidth, AHeight);

  LOG('trying to create a dib section');
  DeviceContext := GetDC(FWindow.WindowHandle);
  If DeviceContext = 0 Then
    Raise TPTCError.Create('could not get device context of window');
  FBitmap := CreateDIBSection(DeviceContext,
                              FBitmapInfo.BMI^,
                              DIB_RGB_COLORS,
                              FBitmapPixels,
                              0, 0);
  ReleaseDC(FWindow.WindowHandle, DeviceContext);
  If FBitmap = 0 Then
    Raise TPTCError.Create('could not create dib section');

  tmp := TPTCArea.Create(0, 0, AWidth, AHeight);
  Try
    FArea.Assign(tmp);
    FClip.Assign(tmp);
  Finally
    tmp.Free;
  End;

  FEventQueue := TEventQueue.Create;
  FKeyboard := TWinCEKeyboard.Create(FEventQueue);
  FMouse := TWinCEMouse.Create(FEventQueue, False, AWidth, AHeight);

  FWindow.Update;

  {todo...}
  FOpen := True;
  LOG('console open succeeded');
End;

Procedure TWinCEGDIConsole.Close;

Begin
  LOG('TWinCEGDIConsole.Close');

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  FBitmapPixels := Nil; { just in case... }
  FreeAndNil(FBitmapInfo);
  If FBitmap <> 0 Then
  Begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  End;
  FreeAndNil(FWindow);

  {todo...}

  FOpen := False;
End;

Procedure TWinCEGDIConsole.Copy(Var ASurface : TPTCBaseSurface);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Copy(Var ASurface : TPTCBaseSurface;
                                Const ASource, ADestination : TPTCArea);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Load(Const APixels : Pointer;
                                AWidth, AHeight, APitch : Integer;
                                Const AFormat : TPTCFormat;
                                Const APalette : TPTCPalette);
Var
  Area_ : TPTCArea;
  console_pixels : Pointer;

Begin
  CheckOpen(    'TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  If Clip.Equals(Area) Then
  Begin
    Try
      console_pixels := Lock;
      Try
        FCopy.Request(AFormat, Format);
        FCopy.Palette(APalette, Palette);
        FCopy.Copy(APixels, 0, 0, AWidth, AHeight, APitch, console_pixels, 0, 0,
                    Width, Height, Pitch);
      Finally
        Unlock;
      End;
    Except
      On error : TPTCError Do
        Raise TPTCError.Create('failed to load pixels to console', error);
    End;
  End
  Else
  Begin
    Area_ := TPTCArea.Create(0, 0, width, height);
    Try
      Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, Area_, Area);
    Finally
      Area_.Free;
    End;
  End;
End;

Procedure TWinCEGDIConsole.Load(Const APixels : Pointer;
                                AWidth, AHeight, APitch : Integer;
                                Const AFormat : TPTCFormat;
                                Const APalette : TPTCPalette;
                                Const ASource, ADestination : TPTCArea);
Var
  console_pixels : Pointer;
  clipped_source, clipped_destination : TPTCArea;
  tmp : TPTCArea;

Begin
  CheckOpen(    'TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TWinCEGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  clipped_source := Nil;
  clipped_destination := Nil;
  Try
    console_pixels := Lock;
    Try
      clipped_source := TPTCArea.Create;
      clipped_destination := TPTCArea.Create;
      tmp := TPTCArea.Create(0, 0, AWidth, AHeight);
      Try
        TPTCClipper.Clip(ASource, tmp, clipped_source, ADestination, Clip, clipped_destination);
      Finally
        tmp.Free;
      End;
      FCopy.request(AFormat, Format);
      FCopy.palette(APalette, Palette);
      FCopy.copy(APixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, APitch,
                 console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, Pitch);
    Finally
      Unlock;
      clipped_source.Free;
      clipped_destination.Free;
    End;
  Except
    On error : TPTCError Do
      Raise TPTCError.Create('failed to load pixels to console area', error);
  End;
End;

Procedure TWinCEGDIConsole.Save(APixels : Pointer;
                                AWidth, AHeight, APitch : Integer;
                                Const AFormat : TPTCFormat;
                                Const APalette : TPTCPalette);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Save(APixels : Pointer;
                                AWidth, AHeight, APitch : Integer;
                                Const AFormat : TPTCFormat;
                                Const APalette : TPTCPalette;
                                Const ASource, ADestination : TPTCArea);

Begin
  {todo...}
End;

Function TWinCEGDIConsole.Lock : Pointer;

Begin
  Result := FBitmapPixels; // todo...
  FLocked := True;
End;

Procedure TWinCEGDIConsole.Unlock;

Begin
  FLocked := False;
End;

Procedure TWinCEGDIConsole.Clear;

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Clear(Const AColor : TPTCColor);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Clear(Const AColor : TPTCColor;
                                 Const AArea : TPTCArea);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Configure(Const AFileName : String);

Var
  F : Text;
  S : String;

Begin
  AssignFile(F, AFileName);
  {$I-}
  Reset(F);
  {$I+}
  If IOResult <> 0 Then
    Exit;
  While Not EoF(F) Do
  Begin
    {$I-}
    Readln(F, S);
    {$I+}
    If IOResult <> 0 Then
      Break;
    Option(S);
  End;
  CloseFile(F);
End;

Function TWinCEGDIConsole.Option(Const AOption : String) : Boolean;

Begin
  LOG('console option', AOption);

  // todo...

  Result := FCopy.Option(AOption);
End;

Procedure TWinCEGDIConsole.Palette(Const APalette : TPTCPalette);

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Clip(Const AArea : TPTCArea);

Var
  tmp : TPTCArea;

Begin
  CheckOpen('TWinCEGDIConsole.Clip(AArea)');

  tmp := TPTCClipper.Clip(AArea, FArea);
  Try
    FClip.Assign(tmp);
  Finally
    tmp.Free;
  End;
End;

Function TWinCEGDIConsole.Clip : TPTCArea;

Begin
  CheckOpen('TWinCEGDIConsole.Clip');
  Result := FClip;
End;

Function TWinCEGDIConsole.Palette : TPTCPalette;

Begin
  {todo...}
End;

Function TWinCEGDIConsole.Modes : PPTCMode;

Begin
  // todo...
  Result := Nil;
End;

Procedure TWinCEGDIConsole.Flush;

Begin
  {todo...}
End;

Procedure TWinCEGDIConsole.Finish;

Begin
  {todo...}
End;

Function TWinCEGDIConsole.WndProc(Ahwnd : HWND; AuMsg : UINT; AwParam : WPARAM; AlParam : LPARAM) : LRESULT;

Begin
  Case AuMsg Of
  WM_CLOSE : Begin
    LOG('TWinCEGDIConsole.WndProc: WM_CLOSE');
    Halt(0);
  End;
  WM_KEYDOWN, WM_KEYUP : Begin
    If FKeyboard <> Nil Then
      Result := FKeyboard.WndProc(Ahwnd, AuMsg, AwParam, AlParam)
    Else
      Result := 0;
    Exit;
  End;
  WM_MOUSEMOVE,
  WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK,
  WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK,
  WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK : Begin
    If FMouse <> Nil Then
      Result := FMouse.WndProc(Ahwnd, AuMsg, AwParam, AlParam)
    Else
      Result := 0;
    Exit;
  End;
  Else
    Result := DefWindowProcW(Ahwnd, AuMsg, AwParam, AlParam);
  End;
End;

Procedure TWinCEGDIConsole.Update;

Var
  ClientRect : RECT;
  DeviceContext, DeviceContext2 : HDC;

Begin
  CheckOpen(    'TWinCEGDIConsole.Update');
  CheckUnlocked('TWinCEGDIConsole.Update');

  FWindow.Update;

  DeviceContext := GetDC(FWindow.WindowHandle);

  If DeviceContext <> 0 Then
  Begin
    If GetClientRect(FWindow.WindowHandle, @ClientRect) Then
    Begin
      DeviceContext2 := CreateCompatibleDC(DeviceContext);
      If DeviceContext2 <> 0 Then
      Begin
        SelectObject(DeviceContext2, FBitmap);

	StretchBlt(DeviceContext,
	           0, 0, ClientRect.right, ClientRect.bottom,
		   DeviceContext2,
		   0, 0, FBitmapInfo.Width, FBitmapInfo.Height,
		   SRCCOPY);

        DeleteDC(DeviceContext2);
      End;
    End;

    ReleaseDC(FWindow.WindowHandle, DeviceContext);
  End;
End;

Procedure TWinCEGDIConsole.Update(Const AArea : TPTCArea);

Begin
  {todo...}
  Update;
End;

Function TWinCEGDIConsole.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

Begin
  CheckOpen('TWinCEGDIConsole.NextEvent');
//  CheckUnlocked('TWinCEGDIConsole.NextEvent');

  FreeAndNil(AEvent);
  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  Until (Not AWait) Or (AEvent <> Nil);
  Result := AEvent <> Nil;
End;

Function TWinCEGDIConsole.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

Begin
  CheckOpen('TWinCEGDIConsole.PeekEvent');
//  CheckUnlocked('TWinCEGDIConsole.PeekEvent');

  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  Until (Not AWait) Or (Result <> Nil);
End;

Function TWinCEGDIConsole.GetWidth : Integer;

Begin
  CheckOpen('TWinCEGDIConsole.GetWidth');
  Result := FBitmapInfo.Width;
End;

Function TWinCEGDIConsole.GetHeight : Integer;

Begin
  CheckOpen('TWinCEGDIConsole.GetHeight');
  Result := FBitmapInfo.Height;
End;

Function TWinCEGDIConsole.GetPitch : Integer;

Begin
  CheckOpen('TWinCEGDIConsole.GetPitch');
  Result := FBitmapInfo.Pitch;
End;

Function TWinCEGDIConsole.GetFormat : TPTCFormat;

Begin
  CheckOpen('TWinCEGDIConsole.GetFormat');
  Result := FBitmapInfo.Format;
End;

Function TWinCEGDIConsole.GetArea : TPTCArea;

Begin
  CheckOpen('TWinCEGDIConsole.GetArea');
  Result := FArea;
End;

Function TWinCEGDIConsole.GetPages : Integer;

Begin
  CheckOpen('TWinCEGDIConsole.GetPages');
  Result := 2;
End;

Function TWinCEGDIConsole.GetName : String;

Begin
  Result := 'WinCE';
End;

Function TWinCEGDIConsole.GetTitle : String;

Begin
  CheckOpen('TWinCEGDIConsole.GetTitle');
  Result := FTitle;
End;

Function TWinCEGDIConsole.GetInformation : String;

Begin
  CheckOpen('TWinCEGDIConsole.GetInformation');
  Result := ''; // todo...
End;

Procedure TWinCEGDIConsole.CheckOpen(AMessage : String);

Begin
  If Not FOpen Then
  Try
    Raise TPTCError.Create('console is not open');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;

Procedure TWinCEGDIConsole.CheckUnlocked(AMessage : String);

Begin
  If FLocked Then
  Try
    Raise TPTCError.Create('console is locked');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;
