Constructor TX11Modes.Create(ADisplay : PDisplay; AScreen : cint);

Begin
  FDisplay := ADisplay;
  FScreen := AScreen;
End;

Constructor TX11ModesNoModeSwitching.Create(ADisplay : PDisplay; AScreen : cint);

Begin
  Inherited;

  FWidth := DisplayWidth(FDisplay, FScreen);
  FHeight := DisplayHeight(FDisplay, FScreen);
End;

Procedure TX11ModesNoModeSwitching.GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat);

Begin
  SetLength(AModes, 2);
  AModes[0] := TPTCMode.Create(FWidth,
                               FHeight,
                               ACurrentDesktopFormat);
  AModes[1] := TPTCMode.Create;
End;

Procedure TX11ModesNoModeSwitching.SetBestMode(AWidth, AHeight : Integer);

Begin
  FWidth := DisplayWidth(FDisplay, FScreen);
  FHeight := DisplayHeight(FDisplay, FScreen);
End;

Procedure TX11ModesNoModeSwitching.RestorePreviousMode;

Begin
  { do nothing }
End;

Function TX11ModesNoModeSwitching.GetWidth : Integer;

Begin
  Result := FWidth;
End;

Function TX11ModesNoModeSwitching.GetHeight : Integer;

Begin
  Result := FHeight;
End;

{$IFDEF ENABLE_X11_EXTENSION_XRANDR}
Constructor TX11ModesXrandr.Create(ADisplay : PDisplay; AScreen : cint);

Var
  dummy1, dummy2 : cint;
  Major, Minor : cint;

Begin
  Inherited;

  If Not XRRQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('Xrandr extension not available');

  XRRQueryVersion(FDisplay, @Major, @Minor); // todo: check
  LOG('Xrandr version: ' + IntToStr(Major) + '.' + IntToStr(Minor));

  FRoot := RootWindow(FDisplay, FScreen);

  FXRRConfig := XRRGetScreenInfo(FDisplay, FRoot);
  If FXRRConfig = Nil Then
    Raise TPTCError.Create('XRRGetScreenInfo failed');

  Raise TPTCError.Create('Xrandr mode switcher is not yet implemented...');
End;

Destructor TX11ModesXrandr.Destroy;

Begin
  If FXRRConfig <> Nil Then
    XRRFreeScreenConfigInfo(FXRRConfig);

  Inherited;
End;

Procedure TX11ModesXrandr.GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat);

Begin
  {...}
End;

Procedure TX11ModesXrandr.SetBestMode(AWidth, AHeight : Integer);

Begin
  {todo...}
End;

Procedure TX11ModesXrandr.RestorePreviousMode;

Begin
  {todo...}
End;

Function TX11ModesXrandr.GetWidth : Integer;

Begin
  // todo...
End;

Function TX11ModesXrandr.GetHeight : Integer;

Begin
  // todo...
End;
{$ENDIF ENABLE_X11_EXTENSION_XRANDR}

{$IFDEF ENABLE_X11_EXTENSION_XF86VIDMODE}
Constructor TX11ModesXF86VidMode.Create(ADisplay : PDisplay; AScreen : Integer);

Var
  dummy1, dummy2 : cint;

Begin
  Inherited;

  FSavedMode := Nil;
  FSavedDotClock := 0;
  FModeList := Nil;
  FModeListCount := 0;

  If Not XF86VidModeQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('VidMode extension not available');
End;

Destructor TX11ModesXF86VidMode.Destroy;

Begin
  If FSavedMode <> Nil Then
  Begin
    RestorePreviousMode;
    If FSavedMode^.privsize <> 0 Then
      XFree(FSavedMode^.c_private);
    Dispose(FSavedMode);
  End;

  If FModeList <> Nil Then
    XFree(FModeList);

  Inherited Destroy;
End;

{todo: move the saving of the previous mode to a separate function...}
Procedure TX11ModesXF86VidMode.RetrieveModeList;

Begin
  { If we have been called before, do nothing }
  If FModeList <> Nil Then
    Exit;

  { Save previous mode }
  New(FSavedMode);
  FillChar(FSavedMode^, SizeOf(FSavedMode^), 0);
  XF86VidModeGetModeLine(FDisplay, FScreen, @FSavedDotClock, FSavedMode);

  { Get all available video modes }
  XF86VidModeGetAllModeLines(FDisplay, FScreen, @FModeListCount, @FModeList);
End;

Procedure TX11ModesXF86VidMode.GetModes(Var AModes : TPTCModeDynArray; ACurrentDesktopFormat : TPTCFormat);

Var
  I : Integer;

Begin
  RetrieveModeList;

  SetLength(AModes, FModeListCount + 1);
  AModes[FModeListCount] := TPTCMode.Create;
  For I := 0 To FModeListCount - 1 Do
    AModes[I] := TPTCMode.Create(FModeList[I]^.hdisplay, FModeList[I]^.vdisplay, ACurrentDesktopFormat);
End;

Function TX11ModesXF86VidMode.FindNumberOfBestMode(AWidth, AHeight : Integer) : Integer;

Var
  min_diff : Integer;
  d_x, d_y : Integer;
  found_mode : Integer;
  I : Integer;

Begin
  { Try an exact match }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay = AWidth) And (FModeList[I]^.vdisplay = AHeight) Then
      Exit(I);

  { Try to find a mode that matches the width first }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay = AWidth) And (FModeList[I]^.vdisplay >= AHeight) Then
      Exit(I);

  { Next try to match the height }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay >= AWidth) And (FModeList[I]^.vdisplay = AHeight) Then
      Exit(I);

  { Finally, find the mode that is bigger than the requested one and makes }
  { the least difference }
  found_mode := -1;
  min_diff := High(Integer);
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay >= AWidth) And (FModeList[I]^.vdisplay >= AHeight) Then
    Begin
      d_x := Sqr(FModeList[I]^.hdisplay - AWidth);
      d_y := Sqr(FModeList[I]^.vdisplay - AHeight);
      If (d_x + d_y) < min_diff Then
      Begin
        min_diff := d_x + d_y;
        found_mode := I;
      End;
    End;

  If found_mode <> -1 Then
    Result := found_mode
  Else
    Raise TPTCError.Create('Cannot find matching video mode');
End;

Procedure TX11ModesXF86VidMode.SetBestMode(AWidth, AHeight : Integer);

Var
  BestMode : Integer;

Begin
  RetrieveModeList;

  BestMode := FindNumberOfBestMode(AWidth, AHeight);
  If Not XF86VidModeSwitchToMode(FDisplay, FScreen, FModeList[BestMode]) Then
    Raise TPTCError.Create('Error switching to the requested video mode');

  FWidth := FModeList[BestMode]^.hdisplay;
  FHeight := FModeList[BestMode]^.vdisplay;

  XWarpPointer(FDisplay, None, RootWindow(FDisplay, FScreen), 0, 0, 0, 0,
               FWidth Div 2,
               FHeight Div 2);

  If Not XF86VidModeSetViewPort(FDisplay, FScreen, 0, 0) Then
    Raise TPTCError.Create('Error moving the viewport to the upper-left corner');
End;

Procedure TX11ModesXF86VidMode.RestorePreviousMode;

Var
  ModeInfo : TXF86VidModeModeInfo;

Begin
  If FSavedMode <> Nil Then
  Begin
    {FSavedMode is a TXF86VidModeModeLine, but XF86VidModeSwitchToMode wants a
                     TXF86VidModeModeInfo :}
    FillChar(ModeInfo, SizeOf(ModeInfo), 0);
    ModeInfo.dotclock := FSavedDotClock;
    ModeInfo.hdisplay := FSavedMode^.hdisplay;
    ModeInfo.hsyncstart := FSavedMode^.hsyncstart;
    ModeInfo.hsyncend := FSavedMode^.hsyncend;
    ModeInfo.htotal := FSavedMode^.htotal;
    ModeInfo.vdisplay := FSavedMode^.vdisplay;
    ModeInfo.vsyncstart := FSavedMode^.vsyncstart;
    ModeInfo.vsyncend := FSavedMode^.vsyncend;
    ModeInfo.vtotal := FSavedMode^.vtotal;
    ModeInfo.flags := FSavedMode^.flags;
    ModeInfo.privsize := FSavedMode^.privsize;
    ModeInfo.c_private := FSavedMode^.c_private;

    XF86VidModeSwitchToMode(FDisplay, FScreen, @ModeInfo);
  End;
End;

Function TX11ModesXF86VidMode.GetWidth : Integer;

Begin
  Result := FWidth;
End;

Function TX11ModesXF86VidMode.GetHeight : Integer;

Begin
  Result := FHeight;
End;
{$ENDIF ENABLE_X11_EXTENSION_XF86VIDMODE}
