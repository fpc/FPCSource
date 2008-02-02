Constructor TX11WindowDisplay.Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags);

Begin
  Inherited;
  FFocus := True;
  FX11InvisibleCursor := None;
  FCursorVisible := True;
End;

Destructor TX11WindowDisplay.Destroy;

Begin
  Close;
  Inherited Destroy;
End;

Procedure TX11WindowDisplay.Open(ATitle : AnsiString; AWidth, AHeight : Integer; Const AFormat : TPTCFormat);

Var
  tmpFormat : TPTCFormat;
  xgcv : TXGCValues;
  textprop : TXTextProperty;
  e : TXEvent;
  found : Boolean;
  attr : TXSetWindowAttributes;
  size_hints : PXSizeHints;
  tmpArea : TPTCArea;
  tmppchar : PChar;
  tmpArrayOfCLong : Array[1..1] Of clong;
  tmpPixmap : TPixmap;
  BlackColor : TXColor;
  BlankCursorData : Array[1..8] Of Byte = (0, 0, 0, 0, 0, 0, 0, 0);

Begin
  FHeight := AHeight;
  FWidth := AWidth;
  FDestX := 0;
  FDestY := 0;

  FFullScreen := PTC_X11_FULLSCREEN In FFlags;

  FFocus := True;

  FPreviousMousePositionSaved := False;

  FillChar(BlackColor, SizeOf(BlackColor), 0);
  BlackColor.red := 0;
  BlackColor.green := 0;
  BlackColor.blue := 0;

  { Create the mode switcher object }
  If (FModeSwitcher = Nil) And FFullScreen Then
    FModeSwitcher := CreateModeSwitcher;

  { Create the invisible cursor }
  tmpPixmap := XCreateBitmapFromData(FDisplay, RootWindow(FDisplay, FScreen), @BlankCursorData, 8, 8);
  Try
    FX11InvisibleCursor := XCreatePixmapCursor(FDisplay, tmpPixmap, tmpPixmap, @BlackColor, @BlackColor, 0, 0);
  Finally
    If tmpPixmap <> None Then
      XFreePixmap(FDisplay, tmpPixmap);
  End;

  { Check if we have that colour depth available.. Easy as there is no
    format conversion yet }
  tmpFormat := Nil;
  Try
    tmpFormat := GetX11Format(AFormat);
    FFormat.Assign(tmpFormat);
  Finally
    tmpFormat.Free;
  End;
  tmpFormat := Nil;

  { Create a window }
  FWindow := XCreateSimpleWindow(FDisplay, RootWindow(FDisplay, FScreen), 0, 0,
                AWidth, AHeight, 0, BlackPixel(FDisplay, FScreen),
                                    BlackPixel(FDisplay, FScreen));
  { Register the delete atom }
  FAtomClose := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', False);
  X11Check(XSetWMProtocols(FDisplay, FWindow, @FAtomClose, 1), 'XSetWMProtocols');
  { Get graphics context }
  xgcv.graphics_exposures := False;
  FGC := XCreateGC(FDisplay, FWindow, GCGraphicsExposures, @xgcv);
  If FGC = Nil Then
    Raise TPTCError.Create('can''t create graphics context');
  { Set window title }
  tmppchar := PChar(ATitle);
  X11Check(XStringListToTextProperty(@tmppchar, 1, @textprop), 'XStringListToTextProperty');
  Try
    XSetWMName(FDisplay, FWindow, @textprop);
    XFlush(FDisplay);
  Finally
    XFree(textprop.value);
  End;

  { Set normal hints }
  size_hints := XAllocSizeHints;
  Try
    size_hints^.flags := PMinSize Or PBaseSize;
    size_hints^.min_width := AWidth;
    size_hints^.min_height := AHeight;
    size_hints^.base_width := AWidth;
    size_hints^.base_height := AHeight;
    If FFullScreen Then
    Begin
      size_hints^.flags := size_hints^.flags Or PWinGravity;
      size_hints^.win_gravity := StaticGravity;
    End
    Else
    Begin
      { not fullscreen - add maxsize limit=minsize, i.e. make window not resizable }
      size_hints^.flags := size_hints^.flags Or PMaxSize;
      size_hints^.max_width := AWidth;
      size_hints^.max_height := AHeight;
    End;
    XSetWMNormalHints(FDisplay, FWindow, size_hints);
    XFlush(FDisplay);
  Finally
    XFree(size_hints);
  End;

  { Set the _NET_WM_STATE property }
  If FFullScreen Then
  Begin
    tmpArrayOfCLong[1] := XInternAtom(FDisplay, '_NET_WM_STATE_FULLSCREEN', False);

    XChangeProperty(FDisplay, FWindow,
                    XInternAtom(FDisplay, '_NET_WM_STATE', False),
                    XA_ATOM,
                    32, PropModeReplace, @tmpArrayOfCLong, 1);
  End;

  { Map the window and wait for success }
  XSelectInput(FDisplay, FWindow, StructureNotifyMask);
  XMapRaised(FDisplay, FWindow);
  Repeat
    XNextEvent(FDisplay, @e);
    If e._type = MapNotify Then
      Break;
  Until False;
  { Get keyboard input and sync }
  XSelectInput(FDisplay, FWindow, KeyPressMask Or KeyReleaseMask Or
                                  StructureNotifyMask Or FocusChangeMask Or
                                  ButtonPressMask Or ButtonReleaseMask Or
                                  PointerMotionMask);
  XSync(FDisplay, False);
  { Create XImage using factory method }
  FPrimary := CreateImage(FDisplay, FScreen, FWidth, FHeight, FFormat);

  found := False;
  Repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(FDisplay, KeyPressMask Or KeyReleaseMask, @e);
  Until Not found;

  attr.backing_store := Always;
  XChangeWindowAttributes(FDisplay, FWindow, CWBackingStore, @attr);

  { Set clipping area }
  tmpArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  Try
    FClip.Assign(tmpArea);
  Finally
    tmpArea.Free;
  End;

  { Installs the right colour map for 8 bit modes }
  CreateColormap;

  If FFullScreen Then
    EnterFullScreen;
End;

Procedure TX11WindowDisplay.Open(AWindow : TWindow; Const AFormat : TPTCFormat);

Begin
End;

Procedure TX11WindowDisplay.Open(AWindow : TWindow; Const AFormat : TPTCFormat; AX, AY, AWidth, AHeight : Integer);

Begin
End;

Procedure TX11WindowDisplay.Close;

Begin
  FreeAndNil(FModeSwitcher);

  {pthreads?!}
  If FCMap <> 0 Then
  Begin
    XFreeColormap(FDisplay, FCMap);
    FCMap := 0;
  End;

  { Destroy XImage and buffer }
  FreeAndNil(FPrimary);
  FreeMemAndNil(FColours);

  { Hide and destroy window }
  If (FWindow <> 0) And (Not (PTC_X11_LEAVE_WINDOW In FFlags)) Then
  Begin
    XUnmapWindow(FDisplay, FWindow);
    XSync(FDisplay, False);

    XDestroyWindow(FDisplay, FWindow);
  End;

  { Free the invisible cursor }
  If FX11InvisibleCursor <> None Then
  Begin
    XFreeCursor(FDisplay, FX11InvisibleCursor);
    FX11InvisibleCursor := None;
  End;
End;

Procedure TX11WindowDisplay.internal_ShowCursor(AVisible : Boolean);

Var
  attr : TXSetWindowAttributes;

Begin
  If AVisible Then
    attr.cursor := None { Use the normal cursor }
  Else
    attr.cursor := FX11InvisibleCursor; { Set the invisible cursor }

  XChangeWindowAttributes(FDisplay, FWindow, CWCursor, @attr);
End;

Procedure TX11WindowDisplay.SetCursor(AVisible : Boolean);

Begin
  FCursorVisible := AVisible;

  If FFocus Then
    internal_ShowCursor(FCursorVisible);
End;

Procedure TX11WindowDisplay.EnterFullScreen;

Begin
  { Try to switch mode }
  If Assigned(FModeSwitcher) Then
    FModeSwitcher.SetBestMode(FWidth, FHeight);

  XSync(FDisplay, False);

  { Center the image }
  FDestX := FModeSwitcher.Width Div 2 - FWidth Div 2;
  FDestY := FModeSwitcher.Height Div 2 - FHeight Div 2;
End;

Procedure TX11WindowDisplay.LeaveFullScreen;

Begin
  { Restore previous mode }
  If Assigned(FModeSwitcher) Then
    FModeSwitcher.RestorePreviousMode;

  XSync(FDisplay, False);
End;

Procedure TX11WindowDisplay.HandleChangeFocus(ANewFocus : Boolean);

Begin
  { No change? }
  If ANewFocus = FFocus Then
    Exit;

  FFocus := ANewFocus;
  If FFocus Then
  Begin
    { focus in }
    If FFullScreen Then
      EnterFullScreen;

    internal_ShowCursor(FCursorVisible);
  End
  Else
  Begin
    { focus out }
    If FFullScreen Then
      LeaveFullScreen;

    internal_ShowCursor(True);
  End;

  XSync(FDisplay, False);
End;

Procedure TX11WindowDisplay.HandleEvents;

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

  Procedure HandleMouseEvent;

  Var
    x, y : cint;
    state : cuint;
    PTCMouseButtonState : TPTCMouseButtonState;

    button : TPTCMouseButton;
    before, after : Boolean;
    cstate : TPTCMouseButtonState;

  Begin
    Case e._type Of
      MotionNotify : Begin
        x := e.xmotion.x;
        y := e.xmotion.y;
        state := e.xmotion.state;
      End;
      ButtonPress, ButtonRelease : Begin
        x := e.xbutton.x;
        y := e.xbutton.y;
        state := e.xbutton.state;
        If e._type = ButtonPress Then
        Begin
          Case e.xbutton.button Of
            Button1 : state := state Or Button1Mask;
            Button2 : state := state Or Button2Mask;
            Button3 : state := state Or Button3Mask;
            Button4 : state := state Or Button4Mask;
            Button5 : state := state Or Button5Mask;
          End;
        End
        Else
        Begin
          Case e.xbutton.button Of
            Button1 : state := state And (Not Button1Mask);
            Button2 : state := state And (Not Button2Mask);
            Button3 : state := state And (Not Button3Mask);
            Button4 : state := state And (Not Button4Mask);
            Button5 : state := state And (Not Button5Mask);
          End;
        End;
      End;
      Else
        Raise TPTCError.Create('Internal Error');
    End;

    If (state And Button1Mask) = 0 Then
      PTCMouseButtonState := []
    Else
      PTCMouseButtonState := [PTCMouseButton1];
    If (state And Button2Mask) <> 0 Then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton2];
    If (state And Button3Mask) <> 0 Then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton3];
    If (state And Button4Mask) <> 0 Then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton4];
    If (state And Button5Mask) <> 0 Then
      PTCMouseButtonState := PTCMouseButtonState + [PTCMouseButton5];

    If (x >= 0) And (x < FWidth) And (y >= 0) And (y < FHeight) Then
    Begin
      If Not FPreviousMousePositionSaved Then
      Begin
        FPreviousMouseX := x; { first DeltaX will be 0 }
        FPreviousMouseY := y; { first DeltaY will be 0 }
        FPreviousMouseButtonState := [];
      End;

      { movement? }
      If (x <> FPreviousMouseX) Or (y <> FPreviousMouseY) Then
        FEventQueue.AddEvent(TPTCMouseEvent.Create(x, y, x - FPreviousMouseX, y - FPreviousMouseY, FPreviousMouseButtonState));

      { button presses/releases? }
      cstate := FPreviousMouseButtonState;
      For button := Low(button) To High(button) Do
      Begin
        before := button In FPreviousMouseButtonState;
        after := button In PTCMouseButtonState;
        If after And (Not before) Then
        Begin
          { button was pressed }
          cstate := cstate + [button];
          FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(x, y, 0, 0, cstate, True, button));
        End
        Else
          If before And (Not after) Then
          Begin
            { button was released }
            cstate := cstate - [button];
            FEventQueue.AddEvent(TPTCMouseButtonEvent.Create(x, y, 0, 0, cstate, False, button));
          End;
      End;

      FPreviousMouseX := x;
      FPreviousMouseY := y;
      FPreviousMouseButtonState := PTCMouseButtonState;
      FPreviousMousePositionSaved := True;
    End;
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
        If (e.xclient.format = 32) And (TAtom(e.xclient.data.l[0]) = FAtomClose) Then
          Halt(0);
      End;
      Expose : Begin
        {...}
      End;
      KeyPress, KeyRelease : HandleKeyEvent;
      ButtonPress, ButtonRelease, MotionNotify : HandleMouseEvent;
    End;
  End;
  If NewFocusSpecified Then
    HandleChangeFocus(NewFocus);
End;

Procedure TX11WindowDisplay.Update;

Begin
  FPrimary.Put(FWindow, FGC, FDestX, FDestY);

  HandleEvents;
End;

Procedure TX11WindowDisplay.Update(Const AArea : TPTCArea);

Var
  updatearea : TPTCArea;
  tmparea : TPTCArea;

Begin
  tmparea := TPTCArea.Create(0, 0, FWidth, FHeight);
  Try
    updatearea := TPTCClipper.Clip(tmparea, AArea);
    Try
      FPrimary.Put(FWindow, FGC, updatearea.Left, updatearea.Top,
                   FDestX + updatearea.Left, FDestY + updatearea.Top,
                   updatearea.Width, updatearea.Height);
    Finally
      updatearea.Free;
    End;
  Finally
    tmparea.Free;
  End;

  HandleEvents;
End;

Function TX11WindowDisplay.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

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

Function TX11WindowDisplay.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

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

Function TX11WindowDisplay.Lock : Pointer;

Begin
  Result := FPrimary.Lock;
End;

Procedure TX11WindowDisplay.unlock;

Begin
End;

Procedure TX11WindowDisplay.GetModes(Var AModes : TPTCModeDynArray);

Var
  current_desktop_format, tmpfmt : TPTCFormat;

Begin
  If FModeSwitcher = Nil Then
    FModeSwitcher := CreateModeSwitcher;

  current_desktop_format := Nil;
  tmpfmt := TPTCFormat.Create(8);
  Try
    current_desktop_format := GetX11Format(tmpfmt);

    FModeSwitcher.GetModes(AModes, current_desktop_format);
  Finally
    tmpfmt.Free;
    current_desktop_format.Free;
  End;
End;

Procedure TX11WindowDisplay.Palette(Const APalette : TPTCPalette);

Var
  pal : PUint32;
  i : Integer;

Begin
  pal := APalette.Data;
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
End;

Function TX11WindowDisplay.GetPitch : Integer;

Begin
  Result := FPrimary.pitch;
End;

Function TX11WindowDisplay.CreateImage(ADisplay : PDisplay; AScreen, AWidth, AHeight : Integer;
                                       AFormat : TPTCFormat) : TX11Image;

Begin
  {$IFDEF ENABLE_X11_EXTENSION_XSHM}
  If (PTC_X11_TRY_XSHM In FFlags) And XShmQueryExtension(ADisplay) Then
  Begin
    Try
      LOG('trying to create a XShm image');
      Result := TX11ShmImage.Create(ADisplay, AScreen, AWidth, AHeight, AFormat);
      Exit;
    Except
      LOG('XShm failed');
    End;
  End;
  {$ENDIF ENABLE_X11_EXTENSION_XSHM}

  LOG('trying to create a normal image');
  Result := TX11NormalImage.Create(ADisplay, AScreen, AWidth, AHeight, AFormat);
End;

Function TX11WindowDisplay.CreateModeSwitcher : TX11Modes;

Begin
{$IFDEF ENABLE_X11_EXTENSION_XRANDR}
  If PTC_X11_TRY_XRANDR In FFlags Then
    Try
      LOG('trying to initialize the Xrandr mode switcher');
      Result := TX11ModesXrandr.Create(FDisplay, FScreen);
      Exit;
    Except
      LOG('Xrandr failed');
    End;
{$ENDIF ENABLE_X11_EXTENSION_XRANDR}

{$IFDEF ENABLE_X11_EXTENSION_XF86VIDMODE}
  If PTC_X11_TRY_XF86VIDMODE In FFlags Then
    Try
      LOG('trying to initialize the XF86VidMode mode switcher');
      Result := TX11ModesXF86VidMode.Create(FDisplay, FScreen);
      Exit;
    Except
      LOG('XF86VidMode failed');
    End;
{$ENDIF ENABLE_X11_EXTENSION_XF86VIDMODE}

  LOG('creating the standard NoModeSwitching mode switcher');
  Result := TX11ModesNoModeSwitching.Create(FDisplay, FScreen);
End;

Function TX11WindowDisplay.GetX11Window : TWindow;

Begin
  Result := FWindow;
End;

Function TX11WindowDisplay.GetX11GC : TGC;

Begin
  Result := FGC;
End;

Function TX11WindowDisplay.IsFullScreen : Boolean;

Begin
  Result := FFullScreen;
End;

Procedure TX11WindowDisplay.CreateColormap; { Register colour maps }

Var
  i : Integer;
  r, g, b : Single;

Begin
  If FFormat.Bits = 8 Then
  Begin
    FColours := GetMem(256 * SizeOf(TXColor));
    If FColours = Nil Then
      Raise TPTCError.Create('Cannot allocate colour map cells');
    FCMap := XCreateColormap(FDisplay, RootWindow(FDisplay, FScreen),
                             DefaultVisual(FDisplay, FScreen), AllocAll);
    If FCMap = 0 Then
      Raise TPTCError.Create('Cannot create colour map');
    XInstallColormap(FDisplay, FCMap);
    XSetWindowColormap(FDisplay, FWindow, FCMap);
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
  End;
End;
