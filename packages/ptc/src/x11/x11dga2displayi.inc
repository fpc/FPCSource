{$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}

Constructor TX11DGA2Display.Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags);

Var
  dummy1, dummy2 : cint;

Begin
  Inherited;

  LOG('trying to open a DGA 2.0 display');

  { Check if the DGA extension can be used }
  LOG('checking if the DGA extension can be used (XDGAQueryExtension)');
  If Not XDGAQueryExtension(FDisplay, @dummy1, @dummy2) Then
    Raise TPTCError.Create('DGA extension not available');
End;

Destructor TX11DGA2Display.Destroy;

Begin
  Close;
  Inherited Destroy;
End;

Procedure TX11DGA2Display.open(title : String; _width, _height : Integer; Const _format : TPTCFormat);

Var
  vml : PXF86VidModeModeLine;
  dotclock : Integer;
  i : Integer;
  found : Boolean;
  root : TWindow;
  e : TXEvent;
  tmpArea : TPTCArea;
  r, g, b : Single;
  found_mode : Integer;
  min_diff : Integer;
  d_x, d_y : Integer;

Begin
  FWidth := _width;
  FHeight := _height;

  LOG('trying to open framebuffer (XDGAOpenFramebuffer)');
  If Not XDGAOpenFramebuffer(FDisplay, FScreen) Then
    Raise TPTCError.Create('Cannot open framebuffer - insufficient privileges?');
  FFramebufferIsOpen := True;

  { Get all availabe video modes }
  LOG('querying available display modes (XDGAQueryModes)');
  FXDGAModes := XDGAQueryModes(FDisplay, FScreen, @FXDGAModesNum);

  LOG('number of display modes', FXDGAModesNum);

  For I := 0 To FXDGAModesNum - 1 Do
  Begin
    LOG('mode#', I);
    LOG('num', FXDGAModes[I].num);
    LOG('name: ' + FXDGAModes[I].name);
  End;

  found_mode := 0; // todo: find a video mode

  Raise TPTCError.Create('break! dga 2.0 code unfinished');

  FXDGADevice := XDGASetMode(FDisplay, FScreen, found_mode);
  If FXDGADevice = Nil Then
    Raise TPTCError.Create('XDGASetMode failed (returned nil)');
  If FXDGADevice^.data = Nil Then
    Raise TPTCError.Create('The pointer to the framebuffer, returned by XDGA is nil?!');
  FModeIsSet := True;

  { Check if the requested colour mode is available }
  FFormat := GetX11Format(_format);

  { Grab exclusive control over the keyboard and mouse }
{  root := XRootWindow(FDisplay, FScreen);
  XGrabKeyboard(FDisplay, root, True, GrabModeAsync, GrabModeAsync, CurrentTime);
  XGrabPointer(FDisplay, root, True, PointerMotionMask Or ButtonPressMask Or
               ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None,
               CurrentTime);}
  XFlush(FDisplay);

  { Get Display information }
{  XF86DGAGetVideo(FDisplay, FScreen, @dga_addr, @dga_linewidth,
                  @dga_banksize, @dga_memsize);}

  { Don't have to be root anymore }
{  setuid(getuid);...}

//  XF86DGAGetViewPortSize(FDisplay, FScreen, @dga_width, @dga_height);

{  If XF86DGAForkApp(FScreen) <> 0 Then
    Raise TPTCError.Create('cannot do safety fork')
  Else
  Begin
    If XF86DGADirectVideo(FDisplay, FScreen, XF86DGADirectGraphics Or
      XF86DGADirectKeyb Or XF86DGADirectMouse) = 0 Then
      Raise TPTCError.Create('cannot switch to DGA mode');
  End;}

//  m_indirect := True;
//  FillChar(dga_addr^, dga_linewidth * dga_height * (FFormat.bits Div 8), 0);

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
  If FFormat.bits = 8 Then
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
  If (FFormat.bits = 8) And FFormat.direct Then
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
Procedure TX11DGA2Display.open(w : TWindow; Const _format : TPTCFormat);

Begin
  If w = 0 Then; { Prevent warnings }
  If _format = Nil Then;
End;

Procedure TX11DGA2Display.open(_window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer);

Begin
  If (_window = 0) Or (_format = Nil) Or (x = 0) Or
     (y = 0) Or (w = 0) Or (h = 0) Then;
End;

Procedure TX11DGA2Display.close;

Var
  tmp : Pointer;

Begin
  If FModeIsSet Then
  Begin
    FModeIsSet := False;

    { restore the original mode }
    XDGASetMode(FDisplay, FScreen, 0); { returns PXDGADevice }
{    XUngrabKeyboard(FDisplay, CurrentTime);
    XUngrabPointer(FDisplay, CurrentTime);}
  End;

  If FFramebufferIsOpen Then
  Begin
    FFramebufferIsOpen := False;
    XDGACloseFramebuffer(FDisplay, FScreen);
  End;

  If FDisplay <> Nil Then
    XFlush(FDisplay);

  If FCMap <> 0 Then
  Begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  End;

  FreeMemAndNil(FColours);

  If FXDGADevice <> Nil Then
  Begin
    tmp := FXDGADevice;
    FXDGADevice := Nil;
    XFree(tmp);
  End;

  If FXDGAModes <> Nil Then
  Begin
    tmp := FXDGAModes;
    FXDGAModes := Nil;
    XFree(tmp);
  End;
End;

Procedure TX11DGA2Display.GetModes(Var AModes : TPTCModeDynArray);

Begin
  SetLength(AModes, 1);
  AModes[0] := TPTCMode.Create;
  {todo...}
End;

Procedure TX11DGA2Display.update;

Begin
End;

Procedure TX11DGA2Display.update(Const _area : TPTCArea);

Begin
End;

Procedure TX11DGA2Display.HandleEvents;

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

Function TX11DGA2Display.NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean;

Var
  tmpEvent : TXEvent;

Begin
  FreeAndNil(event);
  Repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    event := FEventQueue.NextEvent(EventMask);

    If wait And (event = Nil) Then
    Begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    End;
  Until (Not Wait) Or (event <> Nil);
  Result := event <> Nil;
End;

Function TX11DGA2Display.PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent;

Var
  tmpEvent : TXEvent;

Begin
  Repeat
    { process all events from the X queue and put them on our FEventQueue }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(EventMask);

    If wait And (Result = Nil) Then
    Begin
      { if the X event queue is empty, block until an event is received }
      XPeekEvent(FDisplay, @tmpEvent);
    End;
  Until (Not Wait) Or (Result <> Nil);
End;


Function TX11DGA2Display.lock : Pointer;

Begin
  lock := PByte(FXDGADevice^.data) +
          FXDGADevice^.mode.bytesPerScanline * m_desty +
          m_destx * (FXDGADevice^.mode.bitsPerPixel Div 8);
End;

Procedure TX11DGA2Display.unlock;

Begin
End;

Procedure TX11DGA2Display.palette(Const _palette : TPTCPalette);

Var
  pal : PUint32;
  i : Integer;

Begin
  pal := _palette.data;
  If Not FFormat.indexed Then
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

Function TX11DGA2Display.GetPitch : Integer;

Begin
  Result := FXDGADevice^.mode.bytesPerScanline;
End;

Function TX11DGA2Display.getX11Window : TWindow;

Begin
  Result := DefaultRootWindow(FDisplay);
End;

Function TX11DGA2Display.isFullScreen : Boolean;

Begin
  { DGA is always fullscreen }
  Result := True;
End;

Procedure TX11DGA2Display.SetCursor(visible : Boolean);

Begin
  {nothing... raise exception if visible=true?}
End;

{$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}
