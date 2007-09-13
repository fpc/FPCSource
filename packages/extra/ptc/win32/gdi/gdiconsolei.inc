Constructor TGDIConsole.Create;

Begin
  Inherited Create;

  FDefaultWidth := 320;
  FDefaultHeight := 200;
  FDefaultFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;
  FPalette := TPTCPalette.Create;

  FOpen := False;

  { configure console }
  Configure('ptcpas.cfg');
End;

Destructor TGDIConsole.Destroy;

Begin
  Close;

  {...}

  FWin32DIB.Free;
  FWindow.Free;
  FPalette.Free;
  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;
  FArea.Free;
  FClip.Free;
  FDefaultFormat.Free;

  Inherited Destroy;
End;

Procedure TGDIConsole.Open(Const ATitle : String; APages : Integer = 0);

Begin
  Open(ATitle, FDefaultFormat, APages);
End;

Procedure TGDIConsole.Open(Const ATitle : String; Const AFormat : TPTCFormat;
               APages : Integer = 0);

Begin
  Open(ATitle, FDefaultWidth, FDefaultHeight, AFormat, APages);
End;

Procedure TGDIConsole.Open(Const ATitle : String; Const AMode : TPTCMode;
                           APages : Integer = 0);

Begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
End;

Procedure TGDIConsole.Open(Const ATitle : String; AWidth, AHeight : Integer;
               Const AFormat : TPTCFormat; APages : Integer = 0);

Var
  tmp : TPTCArea;

Begin
  If FOpen Then
    Close;

(*  FWindow := TWin32Window.Create('PTC_GDI_FULLSCREEN',
                                 ATitle,
                                 WS_EX_TOPMOST,
                                 DWord(WS_POPUP Or WS_SYSMENU Or WS_VISIBLE), // fpc windows RTL bug - WS_POPUP should be a DWord!!!
                                 SW_NORMAL,
                                 0, 0,
                                 GetSystemMetrics(SM_CXSCREEN),
                                 GetSystemMetrics(SM_CYSCREEN),
                                 False, False);*)

  FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_FIXED',
                                 ATitle,
                                 0,
                                 WS_VISIBLE Or WS_SYSMENU Or WS_CAPTION Or WS_MINIMIZEBOX,
                                 SW_NORMAL,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 AWidth, AHeight,
                                 {m_center_window}False,
                                 False);

(*  FWindow := TWin32Window.Create('PTC_GDI_WINDOWED_RESIZABLE',
                                 ATitle,
                                 0,
                                 WS_OVERLAPPEDWINDOW Or WS_VISIBLE,
                                 SW_NORMAL,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 AWidth, AHeight,
                                 {m_center_window}False,
                                 False);*)

  FWin32DIB := TWin32DIB.Create(AWidth, AHeight);

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  FEventQueue := TEventQueue.Create;
  FKeyboard := TWin32Keyboard.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue);
  FMouse := TWin32Mouse.Create(FWindow.Handle, FWindow.Thread, False, FEventQueue, {FFullScreen}False, AWidth, AHeight);

  tmp := TPTCArea.Create(0, 0, AWidth, AHeight);
  Try
    FArea.Assign(tmp);
    FClip.Assign(tmp);
  Finally
    tmp.Free;
  End;

  FWindow.Update;

  FTitle := ATitle;

  FOpen := True;
End;

Procedure TGDIConsole.Close;

Begin
  If Not FOpen Then
    Exit;

  {...}

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);

  FreeAndNil(FWin32DIB);
  FreeAndNil(FWindow);

  FreeAndNil(FEventQueue);

  FTitle := '';

  FOpen := False;
End;

Procedure TGDIConsole.Copy(Var ASurface : TPTCBaseSurface);

Begin
  // todo...
End;

Procedure TGDIConsole.Copy(Var ASurface : TPTCBaseSurface;
                           Const ASource, ADestination : TPTCArea);

Begin
  // todo...
End;

Procedure TGDIConsole.Load(Const APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette);
Var
  Area_ : TPTCArea;
  console_pixels : Pointer;

Begin
  CheckOpen(    'TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
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

Procedure TGDIConsole.Load(Const APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);
Var
  console_pixels : Pointer;
  clipped_source, clipped_destination : TPTCArea;
  tmp : TPTCArea;

Begin
  CheckOpen(    'TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TGDIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
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

Procedure TGDIConsole.Save(APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette);

Begin
  // todo...
End;

Procedure TGDIConsole.Save(APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);

Begin
  // todo...
End;

Function TGDIConsole.Lock : Pointer;

Begin
  Result := FWin32DIB.Pixels; // todo...
  FLocked := True;
End;

Procedure TGDIConsole.Unlock;

Begin
  FLocked := False;
End;

Procedure TGDIConsole.Clear;

Begin
  // todo...
End;

Procedure TGDIConsole.Clear(Const AColor : TPTCColor);

Begin
  // todo...
End;

Procedure TGDIConsole.Clear(Const AColor : TPTCColor;
                            Const AArea : TPTCArea);

Begin
  // todo...
End;

Procedure TGDIConsole.Configure(Const AFileName : String);

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

Function TGDIConsole.Option(Const AOption : String) : Boolean;

Begin
  // todo...

  Result := FCopy.Option(AOption);
End;

Procedure TGDIConsole.Palette(Const APalette : TPTCPalette);

Begin
  // todo...
End;

Procedure TGDIConsole.Clip(Const AArea : TPTCArea);

Var
  tmp : TPTCArea;

Begin
  CheckOpen('TGDIConsole.Clip(AArea)');

  tmp := TPTCClipper.Clip(AArea, FArea);
  Try
    FClip.Assign(tmp);
  Finally
    tmp.Free;
  End;
End;

Function TGDIConsole.Clip : TPTCArea;

Begin
  CheckOpen('TGDIConsole.Clip');
  Result := FClip;
End;

Function TGDIConsole.Palette : TPTCPalette;

Begin
  CheckOpen('TGDIConsole.Palette');
  Result := FPalette;
End;

Function TGDIConsole.Modes : PPTCMode;

Begin
  // todo...
  Result := Nil;
End;

Procedure TGDIConsole.Flush;

Begin
  CheckOpen(    'TGDIConsole.Flush');
  CheckUnlocked('TGDIConsole.Flush');

  // todo...
End;

Procedure TGDIConsole.Finish;

Begin
  CheckOpen(    'TGDIConsole.Finish');
  CheckUnlocked('TGDIConsole.Finish');

  // todo...
End;

Procedure TGDIConsole.Update;

Var
  ClientRect : RECT;
  DeviceContext : HDC;

Begin
  CheckOpen(    'TGDIConsole.Update');
  CheckUnlocked('TGDIConsole.Update');

  FWindow.Update;

  DeviceContext := GetDC(FWindow.m_window);

  If DeviceContext <> 0 Then
  Begin
    If GetClientRect(FWindow.m_window, @ClientRect) Then
    Begin
      StretchDIBits(DeviceContext,
                    0, 0, ClientRect.right, ClientRect.bottom,
                    0, 0, FWin32DIB.Width, FWin32DIB.Height,
                    FWin32DIB.Pixels,
                    FWin32DIB.BMI^,
                    DIB_RGB_COLORS,
                    SRCCOPY);
    End;

    ReleaseDC(FWindow.m_window, DeviceContext);
  End;
End;

Procedure TGDIConsole.Update(Const AArea : TPTCArea);

Begin
  Update;
End;

Function TGDIConsole.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

Begin
  CheckOpen('TGDIConsole.NextEvent');
//  CheckUnlocked('TGDIConsole.NextEvent');

  FreeAndNil(AEvent);
  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  Until (Not AWait) Or (AEvent <> Nil);
  Result := AEvent <> Nil;
End;

Function TGDIConsole.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

Begin
  CheckOpen('TGDIConsole.PeekEvent');
//  CheckUnlocked('TGDIConsole.PeekEvent');

  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  Until (Not AWait) Or (Result <> Nil);
End;

Function TGDIConsole.GetWidth : Integer;

Begin
  CheckOpen('TGDIConsole.GetWidth');
  Result := FWin32DIB.Width;
End;

Function TGDIConsole.GetHeight : Integer;

Begin
  CheckOpen('TGDIConsole.GetHeight');
  Result := FWin32DIB.Height;
End;

Function TGDIConsole.GetPitch : Integer;

Begin
  CheckOpen('TGDIConsole.GetPitch');
  Result := FWin32DIB.Pitch;
End;

Function TGDIConsole.GetArea : TPTCArea;

Begin
  CheckOpen('TGDIConsole.GetArea');
  Result := FArea;
End;

Function TGDIConsole.GetFormat : TPTCFormat;

Begin
  CheckOpen('TGDIConsole.GetFormat');
  Result := FWin32DIB.Format;
End;

Function TGDIConsole.GetPages : Integer;

Begin
  CheckOpen('TGDIConsole.GetPages');
  Result := 2;
End;

Function TGDIConsole.GetName : String;

Begin
  Result := 'GDI';
End;

Function TGDIConsole.GetTitle : String;

Begin
  CheckOpen('TGDIConsole.GetTitle');
  Result := FTitle;
End;

Function TGDIConsole.GetInformation : String;

Begin
  CheckOpen('TGDIConsole.GetInformation');
  Result := ''; // todo...
End;

Procedure TGDIConsole.CheckOpen(AMessage : String);

Begin
  If Not FOpen Then
  Try
    Raise TPTCError.Create('console is not open');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;

Procedure TGDIConsole.CheckUnlocked(AMessage : String);

Begin
  If FLocked Then
  Try
    Raise TPTCError.Create('console is locked');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;
