Constructor TX11WindowDisplay.Create;

Begin
  m_has_shm := False;
  m_primary := Nil;
  m_window := 0;
  m_colours := Nil;
  m_keypressed := False;
  FFullScreen := False;
  FPreviousMousePositionSaved := False;
  FFocus := True;
  FModeSwitcher := Nil;
  FX11InvisibleCursor := None;
  FCursorVisible := True;
  Inherited Create;
//  XSHM_LoadLibrary;

{$IFDEF HAVE_X11_EXTENSIONS_XSHM}
  m_has_shm := True;
{$ENDIF HAVE_X11_EXTENSIONS_XSHM}
End;

Destructor TX11WindowDisplay.Destroy;

Begin
  close;
//  XSHM_UnloadLibrary;
  Inherited Destroy;
End;

Procedure TX11WindowDisplay.open(title : String; _width, _height : Integer; Const _format : TPTCFormat; disp : PDisplay; screen : Integer);

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
  m_disp := disp;
  m_screen := DefaultScreen(disp);
  m_height := _height;
  m_width := _width;
  m_destx := 0;
  m_desty := 0;
  
  FFullScreen := PTC_X11_FULLSCREEN In m_flags;
  
  FFocus := True;

  FPreviousMousePositionSaved := False;

  FillChar(BlackColor, SizeOf(BlackColor), 0);
  BlackColor.red := 0;
  BlackColor.green := 0;
  BlackColor.blue := 0;

  { Create the mode switcher object }
  If FFullScreen Then
    Try
      FModeSwitcher := TX11Modes.Create(m_disp, m_screen);
    Except
      On error : TPTCError Do
      Begin
        {todo: log the error}
        FModeSwitcher := Nil;
      End;
    End;

  { Create the invisible cursor }
  tmpPixmap := XCreateBitmapFromData(m_disp, RootWindow(m_disp, m_screen), @BlankCursorData, 8, 8);
  Try
    FX11InvisibleCursor := XCreatePixmapCursor(m_disp, tmpPixmap, tmpPixmap, @BlackColor, @BlackColor, 0, 0);
  Finally
    If tmpPixmap <> None Then
      XFreePixmap(m_disp, tmpPixmap);
  End;

  { Check if we have that colour depth available.. Easy as there is no
    format conversion yet }
  tmpFormat := Nil;
  Try
    tmpFormat := getFormat(_format);
    m_format.ASSign(tmpFormat);
  Finally
    tmpFormat.Free;
  End;
  tmpFormat := Nil;
  
  { Create a window }
  m_window := XCreateSimpleWindow(m_disp, RootWindow(m_disp, m_screen), 0, 0,
                _width, _height, 0, BlackPixel(m_disp, m_screen),
		                    BlackPixel(m_disp, m_screen));
  { Register the delete atom }
  m_atom_close := XInternAtom(m_disp, 'WM_DELETE_WINDOW', False);
  X11Check(XSetWMProtocols(m_disp, m_window, @m_atom_close, 1), 'XSetWMProtocols');
  { Get graphics context }
  xgcv.graphics_exposures := False;
  m_gc := XCreateGC(m_disp, m_window, GCGraphicsExposures, @xgcv);
  If m_gc = Nil Then
    Raise TPTCError.Create('can''t create graphics context');
  { Set window title }
  tmppchar := PChar(title);
  X11Check(XStringListToTextProperty(@tmppchar, 1, @textprop), 'XStringListToTextProperty');
  Try
    XSetWMName(m_disp, m_window, @textprop);
    XFlush(m_disp);
  Finally
    XFree(textprop.value);
  End;
  
  { Set normal hints }
  size_hints := XAllocSizeHints;
  Try
    size_hints^.flags := PMinSize Or PBaseSize;
    size_hints^.min_width := _width;
    size_hints^.min_height := _height;
    size_hints^.base_width := _width;
    size_hints^.base_height := _height;
    If FFullScreen Then
    Begin
      size_hints^.flags := size_hints^.flags Or PWinGravity;
      size_hints^.win_gravity := StaticGravity;
    End
    Else
    Begin
      { not fullscreen - add maxsize limit=minsize, i.e. make window not resizable }
      size_hints^.flags := size_hints^.flags Or PMaxSize;
      size_hints^.max_width := _width;
      size_hints^.max_height := _height;
    End;
    XSetWMNormalHints(m_disp, m_window, size_hints);
    XFlush(m_disp);
  Finally
    XFree(size_hints);
  End;
  
  { Set the _NET_WM_STATE property }
  If FFullScreen Then
  Begin
    tmpArrayOfCLong[1] := XInternAtom(m_disp, '_NET_WM_STATE_FULLSCREEN', False);
    
    XChangeProperty(m_disp, m_window, 
                    XInternAtom(m_disp, '_NET_WM_STATE', False),
		    XA_ATOM,
		    32, PropModeReplace, @tmpArrayOfCLong, 1);
  End;
  
  { Map the window and wait for success }
  XSelectInput(m_disp, m_window, StructureNotifyMask);
  XMapRaised(m_disp, m_window);
  Repeat
    XNextEvent(disp, @e);
    If e._type = MapNotify Then
      Break;
  Until False;
  { Get keyboard input and sync }
  XSelectInput(m_disp, m_window, KeyPressMask Or KeyReleaseMask Or
                                 StructureNotifyMask Or FocusChangeMask Or
				 ButtonPressMask Or ButtonReleaseMask Or
				 PointerMotionMask);
  XSync(m_disp, False);
  { Create XImage using factory method }
  m_primary := createImage(m_disp, m_screen, m_width, m_height, m_format);
  
  found := False;
  Repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(m_disp, KeyPressMask Or KeyReleaseMask, @e);
  Until Not found;
  
  attr.backing_store := Always;
  XChangeWindowAttributes(m_disp, m_window, CWBackingStore, @attr);
  
  { Set clipping area }
  tmpArea := TPTCArea.Create(0, 0, m_width, m_height);
  Try
    m_clip.ASSign(tmpArea);
  Finally
    tmpArea.Free;
  End;
  
  { Installs the right colour map for 8 bit modes }
  createColormap;

  If FFullScreen Then
    EnterFullScreen;
End;

Procedure TX11WindowDisplay.open(disp : PDisplay; screen : Integer; w : TWindow; Const _format : TPTCFormat);

Begin
End;

Procedure TX11WindowDisplay.open(disp : PDisplay; screen : Integer; _window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer);

Begin
End;

Procedure TX11WindowDisplay.close;

Begin
  FreeAndNil(FModeSwitcher);

  {pthreads?!}
  If m_cmap <> 0 Then
  Begin
    XFreeColormap(m_disp, m_cmap);
    m_cmap := 0;
  End;
  
  { Destroy XImage and buffer }
  FreeAndNil(m_primary);
  FreeMemAndNil(m_colours);
  
  { Hide and destroy window }
  If (m_window <> 0) And (Not (PTC_X11_LEAVE_WINDOW In m_flags)) Then
  Begin
    XUnmapWindow(m_disp, m_window);
    XSync(m_disp, False);
    
    XDestroyWindow(m_disp, m_window);
  End;
  
  { Free the invisible cursor }
  If FX11InvisibleCursor <> None Then
  Begin
    XFreeCursor(m_disp, FX11InvisibleCursor);
    FX11InvisibleCursor := None;
  End;
End;

Procedure TX11WindowDisplay.internal_ShowCursor(visible : Boolean);

Var
  attr : TXSetWindowAttributes;

Begin
  If visible Then
    attr.cursor := None { Use the normal cursor }
  Else
    attr.cursor := FX11InvisibleCursor; { Set the invisible cursor }
  
  XChangeWindowAttributes(m_disp, m_window, CWCursor, @attr);
End;

Procedure TX11WindowDisplay.SetCursor(visible : Boolean);

Begin
  FCursorVisible := visible;
  
  If FFocus Then
    internal_ShowCursor(FCursorVisible);
End;

Procedure TX11WindowDisplay.EnterFullScreen;

Begin
  { Try to switch mode }
  If Assigned(FModeSwitcher) Then
    FModeSwitcher.SetBestMode(m_width, m_height);

  XSync(m_disp, False);
End;

Procedure TX11WindowDisplay.LeaveFullScreen;

Begin
  { Restore previous mode }
  If Assigned(FModeSwitcher) Then
    FModeSwitcher.RestorePreviousMode;

  XSync(m_disp, False);
End;

Procedure TX11WindowDisplay.HandleChangeFocus(NewFocus : Boolean);

Begin
  { No change? }
  If NewFocus = FFocus Then
    Exit;

  FFocus := NewFocus;
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
  
  XSync(m_disp, False);
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
    If XCheckTypedEvent(m_disp, ClientMessage, @tmpEvent) Then
    Begin
      Result := True;
      XPutBackEvent(m_disp, @tmpEvent);
      Exit;
    End;
    
    If XCheckMaskEvent(m_disp, FocusChangeMask Or
                       KeyPressMask Or KeyReleaseMask Or
		       ButtonPressMask Or ButtonReleaseMask Or
		       PointerMotionMask Or ExposureMask, @tmpEvent) Then
    Begin
      Result := True;
      XPutBackEvent(m_disp, @tmpEvent);
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
      0 : key := TPTCKeyEvent.Create(m_normalkeys[sym And $FF], uni, alt, shift, ctrl, press);
      $FF : key := TPTCKeyEvent.Create(m_functionkeys[sym And $FF], uni, alt, shift, ctrl, press);
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
    
    If (x >= 0) And (x < m_width) And (y >= 0) And (y < m_height) Then
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
    XNextEvent(m_disp, @e);
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
        If (e.xclient.format = 32) And (TAtom(e.xclient.data.l[0]) = m_atom_close) Then
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

Procedure TX11WindowDisplay.update;

Begin
  m_primary.put(m_window, m_gc, m_destx, m_desty);
  
  HandleEvents;
End;

Procedure TX11WindowDisplay.update(Const _area : TPTCArea);

Var
  updatearea : TPTCArea;
  tmparea : TPTCArea;

Begin
  tmparea := TPTCArea.Create(0, 0, m_width, m_height);
  Try
    updatearea := TPTCClipper.clip(tmparea, _area);
    Try
      m_primary.put(m_window, m_gc, updatearea.left, updatearea.top,
                    m_destx + updatearea.left, m_desty + updatearea.top,
		    updatearea.width, updatearea.height);
    Finally
      updatearea.Free;
    End;
  Finally
    tmparea.Free;
  End;
  
  HandleEvents;
End;

Function TX11WindowDisplay.NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean;

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
      XPeekEvent(m_disp, @tmpEvent);
    End;
  Until (Not Wait) Or (event <> Nil);
  Result := event <> Nil;
End;

Function TX11WindowDisplay.PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent;

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
      XPeekEvent(m_disp, @tmpEvent);
    End;
  Until (Not Wait) Or (Result <> Nil);
End;

Function TX11WindowDisplay.lock : Pointer;

Begin
  lock := m_primary.lock;
End;

Procedure TX11WindowDisplay.unlock;

Begin
End;

Procedure TX11WindowDisplay.palette(Const _palette : TPTCPalette);

Var
  pal : PUint32;
  i : Integer;

Begin
  pal := _palette.data;
  If Not m_format.indexed Then
    Exit;
  For i := 0 To 255 Do
  Begin
    m_colours[i].pixel := i;

    m_colours[i].red := ((pal[i] Shr 16) And $FF) Shl 8;
    m_colours[i].green := ((pal[i] Shr 8) And $FF) Shl 8;
    m_colours[i].blue := (pal[i] And $FF) Shl 8;

    Byte(m_colours[i].flags) := DoRed Or DoGreen Or DoBlue;
  End;
  XStoreColors(m_disp, m_cmap, m_colours, 256);
End;

Function TX11WindowDisplay.pitch : Integer;

Begin
  pitch := m_primary.pitch;
End;

Function TX11WindowDisplay.createImage(disp : PDisplay; screen, _width, _height : Integer;
                                       _format : TPTCFormat) : TX11Image;

{$IFDEF HAVE_X11_EXTENSIONS_XSHM}
Var
  tmp : TX11Image;
{$ENDIF}

Begin
  {todo: shm}
  {$IFDEF HAVE_X11_EXTENSIONS_XSHM}
  If m_has_shm And XShmQueryExtension(disp) Then
  Begin
    Try
      tmp := TX11SHMImage.Create(disp, screen, _width, _height, _format);
    Except
      On e : TPTCError Do
        tmp := TX11NormalImage.Create(disp, screen, _width, _height, _format);
    End;
    createImage := tmp;
  End
  Else
  {$ENDIF}
  createImage := TX11NormalImage.Create(disp, screen, _width, _height, _format);
End;

Function TX11WindowDisplay.getX11Window : TWindow;

Begin
  getX11Window := m_window;
End;

Function TX11WindowDisplay.getX11GC : TGC;

Begin
  getX11GC := m_gc;
End;

Function TX11WindowDisplay.isFullScreen : Boolean;

Begin
  Result := FFullScreen;
End;

Procedure TX11WindowDisplay.createColormap; { Register colour maps }

Var
  i : Integer;
  r, g, b : Single;

Begin
  If m_format.bits = 8 Then
  Begin
    m_colours := GetMem(256 * SizeOf(TXColor));
    If m_colours = Nil Then
      Raise TPTCError.Create('Cannot allocate colour map cells');
    m_cmap := XCreateColormap(m_disp, RootWindow(m_disp, m_screen),
                              DefaultVisual(m_disp, m_screen), AllocAll);
    If m_cmap = 0 Then
      Raise TPTCError.Create('Cannot create colour map');
    XInstallColormap(m_disp, m_cmap);
    XSetWindowColormap(m_disp, m_window, m_cmap);
  End
  Else
    m_cmap := 0;

  { Set 332 palette, for now }
  If (m_format.bits = 8) And m_format.direct Then
  Begin
    {Taken from PTC 0.72, i hope it's fine}
    For i := 0 To 255 Do
    Begin
      r := ((i And $E0) Shr 5) * 255 / 7;
      g := ((i And $1C) Shr 2) * 255 / 7;
      b := (i And $03) * 255 / 3;
      
      m_colours[i].pixel := i;
      
      m_colours[i].red := Round(r) Shl 8;
      m_colours[i].green := Round(g) Shl 8;
      m_colours[i].blue := Round(b) Shl 8;
      
      Byte(m_colours[i].flags) := DoRed Or DoGreen Or DoBlue;
    End;
    XStoreColors(m_disp, m_cmap, m_colours, 256);
  End;
End;
