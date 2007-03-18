{$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}

Constructor TX11DGA1Display.Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags);

Var
  dummy1, dummy2 : Integer;

Begin
  Inherited;

  LOG('trying to create a DGA 1.0 display');

  FInDirect := False;
  FInMode := False;
  FModeInfo := Nil;

  { Check if we are root }
  If fpgeteuid <> 0 Then
    Raise TPTCError.Create('Have to be root to switch to DGA mode');

  { Check if the DGA extension and VidMode extension can be used }
  If Not XF86DGAQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('DGA extension not available');
  If Not XF86VidModeQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('VidMode extension not available');
End;

Destructor TX11DGA1Display.Destroy;

Begin
  Close;
  Inherited Destroy;
End;

Procedure TX11DGA1Display.Open(ATitle : String; AWidth, AHeight : Integer; Const AFormat : TPTCFormat);

Var
  vml : PXF86VidModeModeLine;
  dotclock : Integer;
  i : Integer;
  root : TWindow;
  e : TXEvent;
  found : Boolean;
  tmpArea : TPTCArea;
  r, g, b : Single;
  found_mode : Integer;
  min_diff : Integer;
  d_x, d_y : Integer;

Begin
  FWidth := AWidth;
  FHeight := AHeight;

  { Get all availabe video modes }
  XF86VidModeGetAllModeLines(FDisplay, FScreen, @FModeInfoNum, @FModeInfo);

  FPreviousMode := -1;
  { Save previous mode }
  New(vml);
  Try
    XF86VidModeGetModeLine(FDisplay, FScreen, @dotclock, vml);
    Try
      For i := 0 To FModeInfoNum - 1 Do
      Begin
        If (vml^.hdisplay = FModeInfo[i]^.hdisplay) And
           (vml^.vdisplay = FModeInfo[i]^.vdisplay) Then
        Begin
          FPreviousMode := i;
          Break;
        End;
      End;
    Finally
      If vml^.privsize <> 0 Then
        XFree(vml^.c_private);
    End;
  Finally
    Dispose(vml);
  End;
  If FPreviousMode = -1 Then
    Raise TPTCError.Create('Current mode not found in modelist?! Err, this shouldn''t happen :)');

  { Find a video mode to set }

  { Normal modesetting first, find exactly matching mode }
  found_mode := -1;
  For i := 0 To FModeInfoNum - 1 Do
    If (FModeInfo[i]^.hdisplay = AWidth) And (FModeInfo[i]^.vdisplay = AHeight) Then
    Begin
      found_mode := i;
      Break;
    End;

  { Try to find a mode that matches the width first }
  If found_mode = -1 Then
    For i := 0 To FModeInfoNum - 1 Do
      If (FModeInfo[i]^.hdisplay = AWidth) And
         (FModeInfo[i]^.vdisplay >= AHeight) Then
      Begin
        found_mode := i;
        Break;
      End;

  { Next try to match the height }
  If found_mode = -1 Then
    For i := 0 To FModeInfoNum - 1 Do
      If (FModeInfo[i]^.hdisplay >= AWidth) And
         (FModeInfo[i]^.vdisplay = AHeight) Then
      Begin
        found_mode := i;
        Break;
      End;

  If found_mode = -1 Then
  Begin
    { Finally, find the mode that is bigger than the requested one and makes }
    { the least difference }
    min_diff := 987654321;
    For i := 0 To FModeInfoNum - 1 Do
      If (FModeInfo[i]^.hdisplay >= AWidth) And (FModeInfo[i]^.vdisplay >= AHeight) Then
      Begin
        d_x := Sqr(FModeInfo[i]^.hdisplay - AWidth);
        d_y := Sqr(FModeInfo[i]^.vdisplay - AHeight);
        If (d_x + d_y) < min_diff Then
        Begin
          min_diff := d_x + d_y;
          found_mode := i;
        End;
      End;
  End;

  If found_mode = -1 Then
    Raise TPTCError.Create('Cannot find a video mode to use');

  If Not XF86VidModeSwitchToMode(FDisplay, FScreen, FModeInfo[found_mode]) Then
    Raise TPTCError.Create('Error switching to requested video mode');
  FDestX := (FModeInfo[found_mode]^.hdisplay Div 2) - (AWidth Div 2);
  FDestY := (FModeInfo[found_mode]^.vdisplay Div 2) - (AHeight Div 2);

  XFlush(FDisplay);
  FInMode := True;

  { Check if the requested colour mode is available }
  FFormat := GetX11Format(AFormat);

  { Grab exclusive control over the keyboard and mouse }
  root := XRootWindow(FDisplay, FScreen);
  XGrabKeyboard(FDisplay, root, True, GrabModeAsync, GrabModeAsync, CurrentTime);
  XGrabPointer(FDisplay, root, True, PointerMotionMask Or ButtonPressMask Or
               ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None,
               CurrentTime);
  XFlush(FDisplay);

  { Get Display information }
  XF86DGAGetVideo(FDisplay, FScreen, @FDGAAddr, @FDGALineWidth,
                  @FDGABankSize, @FDGAMemSize);

  { Don't have to be root anymore }
{  fpsetuid(fpgetuid);...}

  XF86DGAGetViewPortSize(FDisplay, FScreen, @FDGAWidth, @FDGAHeight);

  If XF86DGAForkApp(FScreen) <> 0 Then
    Raise TPTCError.Create('cannot do safety fork');

  If XF86DGADirectVideo(FDisplay, FScreen, XF86DGADirectGraphics Or
      XF86DGADirectKeyb Or XF86DGADirectMouse) = 0 Then
    Raise TPTCError.Create('cannot switch to DGA mode');

  FInDirect := True;
  FillChar(FDGAAddr^, FDGALineWidth * FDGAHeight * (FFormat.Bits Div 8), 0);

  XSelectInput(FDisplay, DefaultRootWindow(FDisplay),
               KeyPressMask Or KeyReleaseMask);

  XF86DGASetViewPort(FDisplay, FScreen, 0, 0); { Important.. sort of =) }

  found := False;
  Repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(FDisplay, KeyPressMask Or KeyReleaseMask, @e);
  Until Not found;

  { Create colour map in 8 bit mode }
  If FFormat.Bits = 8 Then
  Begin
    FColours := GetMem(256 * SizeOf(TXColor));
    If FColours = Nil Then
      Raise TPTCError.Create('Cannot allocate colour map cells');
    FCMap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen),
                             DefaultVisual(FDisplay, FScreen), AllocAll);
    If FCMap = 0 Then
      Raise TPTCError.Create('Cannot create colour map');
  End
  Else
    FCMap := 0;

  { Set 332 palette, for now }
  If (FFormat.Bits = 8) And FFormat.Direct Then
  Begin
    {Taken from PTC 0.72, i hope it's fine}
    For i := 0 To 255 Do
    Begin
      r := ((i And $E0) Shr 5) * 255 / 7;
      g := ((i And $1C) Shr 2) * 255 / 7;
      b := (i And $03) * 255 / 3;

      FColours[i].pixel := i;

      FColours[i].red := Round(r) Shl 8;
      FColours[i].green := Round(g) Shl 8;
      FColours[i].blue := Round(b) Shl 8;

      Byte(FColours[i].flags) := DoRed Or DoGreen Or DoBlue;
    End;
    XStoreColors(FDisplay, FCMap, FColours, 256);
    XF86DGAInstallColormap(FDisplay, FScreen, FCMap);
  End;

  { Set clipping area }
  tmpArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  Try
    FClip.Assign(tmpArea);
  Finally
    tmpArea.Free;
  End;
End;

{ Not in DGA mode }
Procedure TX11DGA1Display.Open(AWindow : TWindow; Const AFormat : TPTCFormat);

Begin
  If AWindow = 0 Then; { Prevent warnings }
  If AFormat = Nil Then;
End;

Procedure TX11DGA1Display.Open(AWindow : TWindow; Const AFormat : TPTCFormat; AX, AY, AWidth, AHeight : Integer);

Begin
  If (AWindow = 0) Or
     (AFormat = Nil) Or
     (AX = 0) Or
     (AY = 0) Or
     (AWidth = 0) Or
     (AHeight = 0) Then;
End;

Procedure TX11DGA1Display.Close;

Begin
  If FInDirect Then
  Begin
    FInDirect := False;
    XF86DGADirectVideo(FDisplay, FScreen, 0);
  End;

  If FInMode Then
  Begin
    FInMode := False;
    XF86VidModeSwitchToMode(FDisplay, FScreen, FModeInfo[FPreviousMode]);
    XUngrabKeyboard(FDisplay, CurrentTime);
    XUngrabPointer(FDisplay, CurrentTime);
  End;

  If FDisplay <> Nil Then
    XFlush(FDisplay);

  If FCMap <> 0 Then
  Begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  End;

  FreeMemAndNil(FColours);

  If FModeInfo <> Nil Then
  Begin
    XFree(FModeInfo);
    FModeInfo := Nil;
  End;
End;

Procedure TX11DGA1Display.GetModes(Var AModes : TPTCModeDynArray);

Begin
  SetLength(AModes, 1);
  AModes[0] := TPTCMode.Create;
  {todo...}
End;

Procedure TX11DGA1Display.Update;

Begin
End;

Procedure TX11DGA1Display.Update(Const AArea : TPTCArea);

Begin
End;

Procedure TX11DGA1Display.HandleEvents;

Var
  e : TXEvent;
  NewFocus : Boolean;
  NewFocusSpecified : Boolean;

  Function UsefulEventsPending : Boolean;

  Var
    tmpEvent : TXEvent;

  Begin
    If XCheckTypedEvent(FDisplay, ClientMessage, @tmpEvent) Then
    Begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      Exit;
    End;

    If XCheckMaskEvent(FDisplay, FocusChangeMask Or
                       KeyPressMask Or KeyReleaseMask Or
                       ButtonPressMask Or ButtonReleaseMask Or
                       PointerMotionMask Or ExposureMask, @tmpEvent) Then
    Begin
      Result := True;
      XPutBackEvent(FDisplay, @tmpEvent);
      Exit;
    End;

    Result := False;
  End;

  Procedure HandleKeyEvent;

  Var
    sym : TKeySym;
    sym_modded : TKeySym; { modifiers like shift are taken into account here }
    press : Boolean;
    alt, shift, ctrl : Boolean;
    uni : Integer;
    key : TPTCKeyEvent;
    buf : Array[1..16] Of Char;

  Begin
    sym := XLookupKeySym(@e.xkey, 0);
    XLookupString(@e.xkey, @buf, SizeOf(buf), @sym_modded, Nil);
    uni := X11ConvertKeySymToUnicode(sym_modded);
    alt := (e.xkey.state And Mod1Mask) <> 0;
    shift := (e.xkey.state And ShiftMask) <> 0;
    ctrl := (e.xkey.state And ControlMask) <> 0;
    If e._type = KeyPress Then
      press := True
    Else
      press := False;

    key := Nil;
    Case sym Shr 8 Of
      0 : key := TPTCKeyEvent.Create(FNormalKeys[sym And $FF], uni, alt, shift, ctrl, press);
      $FF : key := TPTCKeyEvent.Create(FFunctionKeys[sym And $FF], uni, alt, shift, ctrl, press);
      Else
        key := TPTCKeyEvent.Create;
    End;
    FEventQueue.AddEvent(key);
  End;

Begin
  NewFocusSpecified := False;
  While UsefulEventsPending Do
  Begin
    XNextEvent(FDisplay, @e);
    Case e._type Of
      FocusIn : Begin
        NewFocus := True;
        NewFocusSpecified := True;
      End;
      FocusOut : Begin
        NewFocus := False;
        NewFocusSpecified := True;
      End;
      ClientMessage : Begin
{        If (e.xclient.format = 32) And (TAtom(e.xclient.data.l[0]) = m_atom_close) Then
          Halt(0);}
      End;
      Expose : Begin
        {...}
      End;
      KeyPress, KeyRelease : HandleKeyEvent;
      ButtonPress, ButtonRelease : Begin
        {...}
      End;
      MotionNotify : Begin
        {...}
      End;
    End;
  End;
//  HandleChangeFocus(NewFocus);
End;

Function TX11DGA1Display.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

Var
  tmpEvent : TXEvent;

Begin
  FreeAndNil(AEvent);
  Repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);

    If AWait And (AEvent = Nil) Then
    Begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    End;
  Until (Not AWait) Or (AEvent <> Nil);
  Result := AEvent <> Nil;
End;

Function TX11DGA1Display.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

Var
  tmpEvent : TXEvent;

Begin
  Repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);

    If AWait And (Result = Nil) Then
    Begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    End;
  Until (Not AWait) Or (Result <> Nil);
End;


Function TX11DGA1Display.Lock : Pointer;

Begin
  Result := FDGAAddr + FDGALineWidth * FDestY * (FFormat.Bits Div 8) +
                       FDestX * (FFormat.Bits Div 8);
End;

Procedure TX11DGA1Display.Unlock;

Begin
End;

Procedure TX11DGA1Display.Palette(Const APalette : TPTCPalette);

Var
  pal : PUint32;
  i : Integer;

Begin
  pal := APalette.data;
  If Not FFormat.Indexed Then
    Exit;
  For i := 0 To 255 Do
  Begin
    FColours[i].pixel := i;

    FColours[i].red := ((pal[i] Shr 16) And $FF) Shl 8;
    FColours[i].green := ((pal[i] Shr 8) And $FF) Shl 8;
    FColours[i].blue := (pal[i] And $FF) Shl 8;

    Byte(FColours[i].flags) := DoRed Or DoGreen Or DoBlue;
  End;
  XStoreColors(FDisplay, FCMap, FColours, 256);
  XF86DGAInstallColormap(FDisplay, FScreen, FCMap);
End;

Function TX11DGA1Display.GetPitch : Integer;

Begin
  Result := FDGALineWidth * (FFormat.Bits Div 8);
End;

Function TX11DGA1Display.GetX11Window : TWindow;

Begin
  Result := DefaultRootWindow(FDisplay);
End;

Function TX11DGA1Display.IsFullScreen : Boolean;

Begin
  { DGA is always fullscreen }
  Result := True;
End;

Procedure TX11DGA1Display.SetCursor(AVisible : Boolean);

Begin
  {nothing... raise exception if visible=true?}
End;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
