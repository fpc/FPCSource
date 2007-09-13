Constructor TX11Modes.Create(display : PDisplay; screen : Integer);

Var
  dummy1, dummy2 : cint;

Begin
  FSavedMode := Nil;
  FSavedDotClock := 0;
  FModeList := Nil;
  FModeListCount := 0;

  FDisplay := display;
  FScreen := screen;

  If Not XF86VidModeQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('VidMode extension not available');
End;

Destructor TX11Modes.Destroy;

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

Procedure TX11Modes.GetModes;

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

Function TX11Modes.FindNumberOfBestMode(width, height : Integer) : Integer;

Var
  min_diff : Integer;
  d_x, d_y : Integer;
  found_mode : Integer;
  I : Integer;

Begin
  { Try an exact match }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay = width) And (FModeList[I]^.vdisplay = height) Then
      Exit(I);

  { Try to find a mode that matches the width first }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay = width) And (FModeList[I]^.vdisplay >= height) Then
      Exit(I);

  { Next try to match the height }
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay >= width) And (FModeList[I]^.vdisplay = height) Then
      Exit(I);

  { Finally, find the mode that is bigger than the requested one and makes }
  { the least difference }
  found_mode := -1;
  min_diff := High(Integer);
  For I := 0 To FModeListCount - 1 Do
    If (FModeList[I]^.hdisplay >= width) And (FModeList[I]^.vdisplay >= height) Then
    Begin
      d_x := FModeList[I]^.hdisplay - width;
      d_x *= d_x;
      d_y := FModeList[I]^.vdisplay - height;
      d_y *= d_y;
      If (d_x + d_y) < min_diff Then
      Begin
        min_diff := d_x + d_y;
        found_mode := I;
      End;
    End;

  If found_mode <> -1 Then
    Result := found_mode
  Else
    Raise TPTCError.Create('Cannot find matching DGA video mode');
End;

Procedure TX11Modes.SetBestMode(width, height : Integer);

Var
  BestMode : Integer;

Begin
  GetModes;

  BestMode := FindNumberOfBestMode(width, height);
  If Not XF86VidModeSwitchToMode(FDisplay, FScreen, FModeList[BestMode]) Then
    Raise TPTCError.Create('Error switching to the requested video mode');

  XWarpPointer(FDisplay, None, RootWindow(FDisplay, FScreen), 0, 0, 0, 0,
               FModeList[BestMode]^.hdisplay Div 2,
	       FModeList[BestMode]^.vdisplay Div 2);

  If Not XF86VidModeSetViewPort(FDisplay, FScreen, 0, 0) Then
    Raise TPTCError.Create('Error moving the viewport to the upper-left corner');
End;

Procedure TX11Modes.RestorePreviousMode;

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
