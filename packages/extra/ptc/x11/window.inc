{$IFDEF XStringListToTextProperty_notyetimplemented_in_xutil_pp}
Function XStringListToTextProperty(list : PPChar; count : Integer;
                                   text_prop_return : PXTextProperty) : TStatus; CDecl; External;
{$ENDIF}

Constructor TX11WindowDisplay.Create;

Begin
  m_has_shm := False;
  m_primary := Nil;
  m_window := 0;
  m_colours := Nil;
  m_keypressed := False;
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

Begin
  m_disp := disp;
  m_screen := DefaultScreen(disp);
  m_height := _height;
  m_width := _width;
  m_destx := 0;
  m_desty := 0;
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
  m_window := XCreateSimpleWindow(m_disp, DefaultRootWindow(m_disp), 0, 0,
                _width, _height, 0, BlackPixel(m_disp, DefaultScreen(m_disp)),
		                    BlackPixel(m_disp, DefaultScreen(m_disp)));
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
    size_hints^.flags := PBaseSize;
    size_hints^.base_width := _width;
    size_hints^.base_height := _height;
    XSetWMNormalHints(m_disp, m_window, size_hints);
    XFlush(m_disp);
  Finally
    XFree(size_hints);
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
                                 StructureNotifyMask Or
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

  {ifdef PTHREADS...}
End;

Procedure TX11WindowDisplay.open(disp : PDisplay; screen : Integer; w : TWindow; Const _format : TPTCFormat);

Begin
End;

Procedure TX11WindowDisplay.open(disp : PDisplay; screen : Integer; _window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer);

Begin
End;

Procedure TX11WindowDisplay.close;

Begin
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
  If (m_window <> 0) And ((m_flags And PTC_X11_LEAVE_WINDOW) = 0) Then
  Begin
    XUnmapWindow(m_disp, m_window);
    XSync(m_disp, False);
    
    XDestroyWindow(m_disp, m_window);
  End;
End;

Procedure TX11WindowDisplay.update;

Var
  e : TXEvent;

Begin
  m_primary.put(m_window, m_gc, m_destx, m_desty);
  {ifndef pthreads}
  If XCheckTypedEvent(m_disp, ClientMessage, @e) Then
  Begin
    If (e.xclient.format = 32) And (TAtom(e.xclient.data.l[0]) = m_atom_close) Then
      Halt(0);
  End;
  {endif}
End;

Procedure TX11WindowDisplay.update(Const _area : TPTCArea);

Var
  e : TXEvent;
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
  
  {ifndef pthreads}
  If XCheckTypedEvent(m_disp, ClientMessage, @e) Then
  Begin
    If (e.xclient.format = 32) And (TAtom(e.xclient.data.l[0]) = m_atom_close) Then
      Halt(0);
  End;
  {endif}
End;

Procedure TX11WindowDisplay.internal_ReadKey(k : TPTCKey);

Var
  e : TXEvent;
  sym : TKeySym;
  press : Boolean;
  alt, shift, ctrl : Boolean;
  uni : Integer;
  tmpkey : TPTCKey;

Begin
  XMaskEvent(m_disp, KeyPressMask Or KeyReleaseMask, @e); { Blocks and waits }
  If (e._type <> KeyPress) And (e._type <> KeyRelease) Then
    Raise TPTCError.Create('XMaskEvent returned event <> KeyPress/KeyRelease');
  
{  XLookupString(@e.xkey, Nil, 0, @sym, Nil);}
  sym := XLookupKeySym(@e.xkey, 0);
  uni := X11ConvertKeySymToUnicode(sym);
  alt := (e.xkey.state And Mod1Mask) <> 0;
  shift := (e.xkey.state And ShiftMask) <> 0;
  ctrl := (e.xkey.state And ControlMask) <> 0;
  If e._type = KeyPress Then
    press := True
  Else
    press := False;

  tmpkey := Nil;
  Try
    Case sym Shr 8 Of
      0 : tmpkey := TPTCKey.Create(m_normalkeys[sym And $FF], uni, alt, shift, ctrl, press);
      $FF : tmpkey := TPTCKey.Create(m_functionkeys[sym And $FF], uni, alt, shift, ctrl, press);
      Else
        tmpkey := TPTCKey.Create;
    End;
    k.ASSign(tmpkey);
  Finally
    tmpkey.Free;
  End;
End;

Function TX11WindowDisplay.internal_PeekKey(k : TPTCKey) : Boolean;

Var
  e : TXEvent;

Begin
  If XCheckMaskEvent(m_disp, KeyPressMask Or KeyReleaseMask, @e) Then
  Begin
    XPutBackEvent(m_disp, @e); { Simulate "normal" kbhit behaviour }
    XPutBackEvent(m_disp, @e); { i.e. leave the buffer intact }
    internal_ReadKey(k);
    Result := True;
  End
  Else
    Result := False;
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
  pal : Pint32;
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
