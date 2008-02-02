Constructor TWinCEGAPIConsole.Create;

Begin
  Inherited Create;

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;

  LOG('getting display properties');
  FGXDisplayProperties := GXGetDisplayProperties;
  LOG('width='  + IntToStr(FGXDisplayProperties.cxWidth ));
  LOG('height=' + IntToStr(FGXDisplayProperties.cyHeight));
  LOG('xpitch=' + IntToStr(FGXDisplayProperties.cbxPitch));
  LOG('ypitch=' + IntToStr(FGXDisplayProperties.cbyPitch));
  LOG('BPP='    + IntToStr(FGXDisplayProperties.cBPP    ));
  LOG('format=' + IntToStr(FGXDisplayProperties.ffFormat));

  FDisplayWidth := FGXDisplayProperties.cxWidth;
  FDisplayHeight := FGXDisplayProperties.cyHeight;
  FDisplayPitch := FGXDisplayProperties.cbyPitch;
//  FDisplayFormat := TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF);
  FDisplayFormat := TPTCFormat.Create(16, $F800, $07E0, $001F); {hardcoded for now...}

  FModes[0] := TPTCMode.Create(FDisplayWidth, FDisplayHeight, FDisplayFormat);
  FModes[1] := TPTCMode.Create;
End;

Destructor TWinCEGAPIConsole.Destroy;

Var
  I : Integer;

Begin
  Close;

  FCopy.Free;
  FClear.Free;
  FArea.Free;
  FClip.Free;
  FDisplayFormat.Free;

  For I := Low(FModes) To High(FModes) Do
    FModes[I].Free;

  Inherited Destroy;
End;

Procedure TWinCEGAPIConsole.Open(Const ATitle : String; APages : Integer = 0);

Begin
  Open(ATitle, FDisplayFormat, APages);
End;

Procedure TWinCEGAPIConsole.Open(Const ATitle : String; Const AFormat : TPTCFormat;
                                 APages : Integer = 0);

Begin
  Open(ATitle, FDisplayWidth, FDisplayHeight, AFormat, APages);
End;

Procedure TWinCEGAPIConsole.Open(Const ATitle : String; Const AMode : TPTCMode;
                                 APages : Integer = 0);

Begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
End;

Procedure TWinCEGAPIConsole.Open(Const ATitle : String; AWidth, AHeight : Integer;
                                 Const AFormat : TPTCFormat; APages : Integer = 0);

Var
  tmp : TPTCArea;

Begin
  LOG('TWinCEGAPIConsole.Open');

  If FOpen Then
    Close;

  Try
    LOG('creating window');
    FWindow := TWinCEWindow.Create('PTC_GAPI_FULLSCREEN',
                                   ATitle,
                                   0,
                                   WS_VISIBLE {Or WS_SYSMENU Or WS_CAPTION},
                                   SW_SHOWNORMAL,
                                   CW_USEDEFAULT, CW_USEDEFAULT,
                                   FDisplayWidth, FDisplayHeight,
				   @WndProc);
    LOG('window created successfully');

    LOG('opening display');
    If GXOpenDisplay(FWindow.WindowHandle, GX_FULLSCREEN) <> 0 Then
      FGXDisplayIsOpen := True {success!!!}
    Else
      Raise TPTCError.Create('could not open display');

    tmp := TPTCArea.Create(0, 0, FDisplayWidth, FDisplayHeight);
    Try
      FArea.Assign(tmp);
      FClip.Assign(tmp);
    Finally
      tmp.Free;
    End;

    FEventQueue := TEventQueue.Create;
    FKeyboard := TWinCEKeyboard.Create(FEventQueue);
    FMouse := TWinCEMouse.Create(FEventQueue, True, FDisplayWidth, FDisplayHeight);

    If {m_primary.m_fullscreen}True Then
      FMouse.SetWindowArea(0, 0, FDisplayWidth, FDisplayHeight);

    FWindow.Update;

    FOpen := True;
  Except
    On error : TObject Do
    Begin
      Close;
      Raise;
    End;
  End;
End;

Procedure TWinCEGAPIConsole.Close;

Begin
  LOG('TWinCEGAPIConsole.Close');

  If FGXDisplayIsOpen Then;
    GXCloseDisplay;
  FGXDisplayIsOpen := False;

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FWindow);
  FreeAndNil(FEventQueue);

  FOpen := False;
End;

Procedure TWinCEGAPIConsole.Copy(Var ASurface : TPTCBaseSurface);

Begin
End;

Procedure TWinCEGAPIConsole.Copy(Var ASurface : TPTCBaseSurface;
                                 Const ASource, ADestination : TPTCArea);

Begin
End;

Procedure TWinCEGAPIConsole.Load(Const APixels : Pointer;
                                 AWidth, AHeight, APitch : Integer;
                                 Const AFormat : TPTCFormat;
                                 Const APalette : TPTCPalette);
Var
  Area_ : TPTCArea;
  console_pixels : Pointer;

Begin
  CheckOpen(    'TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
  CheckUnlocked('TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette)');
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

Procedure TWinCEGAPIConsole.Load(Const APixels : Pointer;
                                 AWidth, AHeight, APitch : Integer;
                                 Const AFormat : TPTCFormat;
                                 Const APalette : TPTCPalette;
                                 Const ASource, ADestination : TPTCArea);
Var
  console_pixels : Pointer;
  clipped_source, clipped_destination : TPTCArea;
  tmp : TPTCArea;

Begin
  CheckOpen(    'TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
  CheckUnlocked('TWinCEGAPIConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination)');
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

Procedure TWinCEGAPIConsole.Save(APixels : Pointer;
                                 AWidth, AHeight, APitch : Integer;
                                 Const AFormat : TPTCFormat;
                                 Const APalette : TPTCPalette);

Begin
End;

Procedure TWinCEGAPIConsole.Save(APixels : Pointer;
                                 AWidth, AHeight, APitch : Integer;
                                 Const AFormat : TPTCFormat;
                                 Const APalette : TPTCPalette;
                                 Const ASource, ADestination : TPTCArea);

Begin
End;

Function TWinCEGAPIConsole.Lock : Pointer;

Begin
  CheckUnlocked('display already locked');
  Result := GXBeginDraw;

  If Result = Nil Then
    Raise TPTCError.Create('the display cannot be locked');

  FLocked := True;
End;

Procedure TWinCEGAPIConsole.Unlock;

Begin
  If Not FLocked Then
    Raise TPTCError.Create('display is not locked');

  If GXEndDraw = 0 Then
    Raise TPTCError.Create('could not unlock display');

  FLocked := False;
End;

Procedure TWinCEGAPIConsole.Clear;

Begin
End;

Procedure TWinCEGAPIConsole.Clear(Const AColor : TPTCColor);

Begin
End;

Procedure TWinCEGAPIConsole.Clear(Const AColor : TPTCColor;
                    Const AArea : TPTCArea);

Begin
End;

Procedure TWinCEGAPIConsole.Configure(Const AFileName : String);

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

Function TWinCEGAPIConsole.Option(Const AOption : String) : Boolean;

Begin
  LOG('console option', AOption);

  // todo...

  Result := FCopy.Option(AOption);
End;

Procedure TWinCEGAPIConsole.Palette(Const APalette : TPTCPalette);

Begin
End;

Procedure TWinCEGAPIConsole.Clip(Const AArea : TPTCArea);

Var
  tmp : TPTCArea;

Begin
  CheckOpen('TWinCEGAPIConsole.Clip(AArea)');

  tmp := TPTCClipper.Clip(AArea, FArea);
  Try
    FClip.Assign(tmp);
  Finally
    tmp.Free;
  End;
End;

Function TWinCEGAPIConsole.Clip : TPTCArea;

Begin
  CheckOpen('TWinCEGAPIConsole.Clip');
  Result := FClip;
End;

Function TWinCEGAPIConsole.Palette : TPTCPalette;

Begin
End;

Function TWinCEGAPIConsole.Modes : PPTCMode;

Begin
  Result := @FModes[0];
End;

Function TWinCEGAPIConsole.WndProc(Ahwnd : HWND; AuMsg : UINT; AwParam : WPARAM; AlParam : LPARAM) : LRESULT;

Begin
  Case AuMsg Of
  WM_CLOSE : Begin
    LOG('TWinCEGAPIConsole.WndProc: WM_CLOSE');
    Halt(0);
  End;
  WM_KILLFOCUS : Begin
    LOG('TWinCEGAPIConsole.WndProc: WM_KILLFOCUS');
    If FGXDisplayIsOpen Then
      GXSuspend;
    Result := 0;
    Exit;
  End;
  WM_SETFOCUS : Begin
    LOG('TWinCEGAPIConsole.WndProc: WM_SETFOCUS');
    If FGXDisplayIsOpen Then
      GXResume;
    Result := 0;
    Exit;
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

Procedure TWinCEGAPIConsole.Flush;

Begin
  CheckOpen    ('TWinCEGAPIConsole.Flush');
  CheckUnlocked('TWinCEGAPIConsole.Flush');

  Update;
End;

Procedure TWinCEGAPIConsole.Finish;

Begin
  CheckOpen    ('TWinCEGAPIConsole.Finish');
  CheckUnlocked('TWinCEGAPIConsole.Finish');

  Update;
End;

Procedure TWinCEGAPIConsole.Update;

Begin
  CheckOpen    ('TWinCEGAPIConsole.Update');
  CheckUnlocked('TWinCEGAPIConsole.Update');

  FWindow.Update;
End;

Procedure TWinCEGAPIConsole.Update(Const AArea : TPTCArea);

Begin
  Update;
End;

Function TWinCEGAPIConsole.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

Begin
  CheckOpen('TWinCEGAPIConsole.NextEvent');
//  CheckUnlocked('TWinCEGAPIConsole.NextEvent');

  FreeAndNil(AEvent);
  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  Until (Not AWait) Or (AEvent <> Nil);
  Result := AEvent <> Nil;
End;

Function TWinCEGAPIConsole.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

Begin
  CheckOpen('TWinCEGAPIConsole.PeekEvent');
//  CheckUnlocked('TWinCEGAPIConsole.PeekEvent');

  Repeat
    { update window }
    FWindow.Update;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  Until (Not AWait) Or (Result <> Nil);
End;

Function TWinCEGAPIConsole.GetWidth : Integer;

Begin
  CheckOpen('TWinCEGAPIConsole.GetWidth');
  Result := FDisplayWidth;
End;

Function TWinCEGAPIConsole.GetHeight : Integer;

Begin
  CheckOpen('TWinCEGAPIConsole.GetHeight');
  Result := FDisplayHeight;
End;

Function TWinCEGAPIConsole.GetPitch : Integer;

Begin
  CheckOpen('TWinCEGAPIConsole.GetPitch');
  Result := FDisplayPitch;
End;

Function TWinCEGAPIConsole.GetArea : TPTCArea;

Begin
  CheckOpen('TWinCEGAPIConsole.GetArea');
  Result := FArea;
End;

Function TWinCEGAPIConsole.GetFormat : TPTCFormat;

Begin
  CheckOpen('TWinCEGAPIConsole.GetFormat');
  Result := FDisplayFormat;
End;

Function TWinCEGAPIConsole.GetPages : Integer;

Begin
  CheckOpen('TWinCEGAPIConsole.GetPages');
  Result := 1; {???}
End;

Function TWinCEGAPIConsole.GetName : String;

Begin
  Result := 'GAPI';
End;

Function TWinCEGAPIConsole.GetTitle : String;

Begin
  CheckOpen('TWinCEGAPIConsole.GetTitle');
  Result := FTitle;
End;

Function TWinCEGAPIConsole.GetInformation : String;

Begin
  Result := ''; // todo...
End;

Procedure TWinCEGAPIConsole.CheckOpen(    AMessage : String);

Begin
  If Not FOpen Then
  Try
    Raise TPTCError.Create('console is not open');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;

Procedure TWinCEGAPIConsole.CheckUnlocked(AMessage : String);

Begin
  If FLocked Then
  Try
    Raise TPTCError.Create('console is locked');
  Except
    On error : TPTCError Do
      Raise TPTCError.Create(AMessage, error);
  End;
End;
