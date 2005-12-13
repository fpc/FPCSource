Constructor TX11DGADisplay.Create;

Begin
  m_indirect := False;
  m_inmode := False;
  modeinfo := Nil;
  Inherited Create;
  
//  dga_LoadLibrary;

{  If (XF86DGAQueryExtension = Nil) Or (XF86DGAGetVideo = Nil) Or
     (XF86DGAGetViewPortSize = Nil) Or (XF86DGAForkApp = Nil) Or
     (XF86DGADirectVideo = Nil) Or (XF86DGASetViewPort = Nil) Or
     (XF86DGAInstallColormap = Nil) Then
    Raise TPTCError.Create('DGA extension not available');}
End;

Destructor TX11DGADisplay.Destroy;

Begin
  close; {fix close!}
//  dga_UnloadLibrary;
  Inherited Destroy;
End;

Procedure TX11DGADisplay.open(title : String; _width, _height : Integer; Const _format : TPTCFormat; disp : PDisplay; screen : Integer);

Var
  dummy1, dummy2 : Integer;
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
  m_disp := disp;
  m_screen := screen;
  m_width := _width;
  m_height := _height;
  
  { Check if we are root }
  If fpgeteuid <> 0 Then
    Raise TPTCError.Create('Have to be root to switch to DGA mode');
  
  { Check if the DGA extension and VidMode extension can be used }
  If Not XF86DGAQueryExtension(disp, @dummy1, @dummy2) Then
    Raise TPTCError.Create('DGA extension not available');
  If Not XF86VidModeQueryExtension(disp, @dummy1, @dummy2) Then
    Raise TPTCError.Create('VidMode extension not available');

  { Get all availabe video modes }
  XF86VidModeGetAllModeLines(m_disp, m_screen, @num_modeinfo, @modeinfo);

  previousmode := -1;
  { Save previous mode }
  New(vml);
  Try
    XF86VidModeGetModeLine(m_disp, m_screen, @dotclock, vml);
    Try
      For i := 0 To num_modeinfo - 1 Do
      Begin
        If (vml^.hdisplay = modeinfo[i]^.hdisplay) And
           (vml^.vdisplay = modeinfo[i]^.vdisplay) Then
        Begin
          previousmode := i;
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
  If previousmode = -1 Then
    Raise TPTCError.Create('Current mode not found in modelist?! Err, this shouldn''t happen :)');
  
  { Find a video mode to set }
  
  { Normal modesetting first, find exactly matching mode }
  If (m_flags And PTC_X11_PEDANTIC_DGA) = 0 Then
  Begin
    found := False;
    For i := 0 To num_modeinfo - 1 Do
    Begin
      If (modeinfo[i]^.hdisplay = _width) And (modeinfo[i]^.vdisplay = _height) Then
      Begin
        If Not XF86VidModeSwitchToMode(m_disp, m_screen, modeinfo[i]) Then
          Raise TPTCError.Create('Error switching to requested video mode');
        m_destx := 0;
        m_desty := 0;
        found := True;
        Break;
      End;
    End;
    If Not found Then
      Raise TPTCError.Create('Cannot find matching DGA video mode');
  End
  Else
  Begin
    found_mode := $FFFF;
    
    { Try to find a mode that matches the width first }
    For i := 0 To num_modeinfo - 1 Do
    Begin
      If (modeinfo[i]^.hdisplay = _width) And
         (modeinfo[i]^.vdisplay >= _height) Then
      Begin
        found_mode := i;
        Break;
      End;
    End;
    
    { Next try to match the height }
    If found_mode = $FFFF Then
      For i := 0 To num_modeinfo - 1 Do
      Begin
        If (modeinfo[i]^.hdisplay >= _width) And
           (modeinfo[i]^.vdisplay = _height) Then
        Begin
          found_mode := i;
          Break;
        End;
      End;
    
    { Finally, find the mode that is bigger than the requested one and makes }
    { the least difference }
    min_diff := 987654321;
    
    For i := 0 To num_modeinfo - 1 Do
    Begin
      If (modeinfo[i]^.hdisplay >= _width) And (modeinfo[i]^.vdisplay >= _height) Then
      Begin
        d_x := sqr(modeinfo[i]^.hdisplay - _width);
        d_y := sqr(modeinfo[i]^.vdisplay - _height);
        If (d_x + d_y) < min_diff Then
        Begin
          min_diff := d_x + d_y;
          found_mode := i;
        End;
      End;
    End;
    
    If found_mode <> $FFFF Then
    Begin
      If Not XF86VidModeSwitchToMode(m_disp, m_screen, modeinfo[found_mode]) Then
        Raise TPTCError.Create('Error switching to requested video mode');
      m_destx := (modeinfo[found_mode]^.hdisplay Div 2) - (_width Div 2);
      m_desty := (modeinfo[found_mode]^.vdisplay Div 2) - (_height Div 2);
    End
    Else
      Raise TPTCError.Create('Cannot find a video mode to use');
  End;
  XFlush(m_disp);
  m_inmode := True;

  { Check if the requested colour mode is available }
  m_format := getFormat(_format);
  
  { Grab exclusive control over the keyboard and mouse }
  root := XRootWindow(m_disp, m_screen);
  XGrabKeyboard(m_disp, root, True, GrabModeAsync, GrabModeAsync, CurrentTime);
  XGrabPointer(m_disp, root, True, PointerMotionMask Or ButtonPressMask Or
               ButtonReleaseMask, GrabModeAsync, GrabModeAsync, None, None,
               CurrentTime);
  XFlush(m_disp);
  
  { Get Display information }
  XF86DGAGetVideo(m_disp, m_screen, @dga_addr, @dga_linewidth,
                  @dga_banksize, @dga_memsize);
  
  { Don't have to be root anymore }
{  setuid(getuid);...}
  
  XF86DGAGetViewPortSize(m_disp, m_screen, @dga_width, @dga_height);
  
  If XF86DGAForkApp(m_screen) <> 0 Then
    Raise TPTCError.Create('cannot do safety fork')
  Else
  Begin
    If XF86DGADirectVideo(m_disp, m_screen, XF86DGADirectGraphics Or
      XF86DGADirectKeyb Or XF86DGADirectMouse) = 0 Then
      Raise TPTCError.Create('cannot switch to DGA mode');
  End;
  
  m_indirect := True;
  FillChar(dga_addr^, dga_linewidth * dga_height * (m_format.bits Div 8), 0);
  
  XSelectInput(m_disp, DefaultRootWindow(m_disp),
               KeyPressMask Or KeyReleaseMask);
  
  XF86DGASetViewPort(m_disp, m_screen, 0, 0); { Important.. sort of =) }

  found := False;
  Repeat
    { Stupid loop. The key }
    { events were causing }
    { problems.. }
    found := XCheckMaskEvent(m_disp, KeyPressMask Or KeyReleaseMask, @e);
  Until Not found;
  
  { Create colour map in 8 bit mode }
  If m_format.bits = 8 Then
  Begin
    m_colours := GetMem(256 * SizeOf(TXColor));
    If m_colours = Nil Then
      Raise TPTCError.Create('Cannot allocate colour map cells');
    m_cmap := XCreateColormap(m_disp, RootWindow(m_disp, m_screen),
                              DefaultVisual(m_disp, m_screen), AllocAll);
    If m_cmap = 0 Then
      Raise TPTCError.Create('Cannot create colour map');
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
    XF86DGAInstallColormap(m_disp, m_screen, m_cmap);
  End;
  
  { Set clipping area }
  tmpArea := TPTCArea.Create(0, 0, m_width, m_height);
  Try
    m_clip.ASSign(tmpArea);
  Finally
    tmpArea.Free;
  End;
End;

{ Not in DGA mode }
Procedure TX11DGADisplay.open(disp : PDisplay; screen : Integer; w : TWindow; Const _format : TPTCFormat);

Begin
  If disp = Nil Then; { Prevent warnings }
  If screen = 0 Then;
  If w = 0 Then;
  If _format = Nil Then;
End;

Procedure TX11DGADisplay.open(disp : PDisplay; screen : Integer; _window : TWindow; Const _format : TPTCFormat; x, y, w, h : Integer);

Begin
  If (disp = Nil) Or (screen = 0) Or (_window = 0) Or (_format = Nil) Or (x = 0) Or
     (y = 0) Or (w = 0) Or (h = 0) Then;
End;

Procedure TX11DGADisplay.close;

Begin
  If m_indirect Then
  Begin
    m_indirect := False;
    XF86DGADirectVideo(m_disp, m_screen, 0);
  End;
  
//  Writeln('lala1');
  If m_inmode Then
  Begin
    m_inmode := False;
    XF86VidModeSwitchToMode(m_disp, m_screen, modeinfo[previousmode]);
    XUngrabKeyboard(m_disp, CurrentTime);
    XUngrabPointer(m_disp, CurrentTime);
  End;
  
//  Writeln('lala2');
  If m_disp <> Nil Then
    XFlush(m_disp);
//  Writeln('lala3');

  If m_cmap <> 0 Then
  Begin
    XFreeColormap(m_disp, m_cmap);
    m_cmap := 0;
  End;
  
//  Writeln('lala4');
  FreeMemAndNil(m_colours);
  
//  Writeln('lala5');
  If modeinfo <> Nil Then
  Begin
    XFree(modeinfo);
    modeinfo := Nil;
  End;
//  Writeln('lala6');
End;

Procedure TX11DGADisplay.update;

Begin
End;

Procedure TX11DGADisplay.update(Const _area : TPTCArea);

Begin
End;

Procedure TX11DGADisplay.internal_ReadKey(k : TPTCKey);

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

Function TX11DGADisplay.internal_PeekKey(k : TPTCKey) : Boolean;

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

Function TX11DGADisplay.lock : Pointer;

Begin
  lock := dga_addr + dga_linewidth * m_desty * (m_format.bits Div 8) +
                     m_destx * (m_format.bits Div 8);
End;

Procedure TX11DGADisplay.unlock;

Begin
End;

Procedure TX11DGADisplay.palette(Const _palette : TPTCPalette);

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
  XF86DGAInstallColormap(m_disp, m_screen, m_cmap);
End;

Function TX11DGADisplay.pitch : Integer;

Begin
  pitch := dga_linewidth * (m_format.bits Div 8);
End;

Function TX11DGADisplay.getX11Window : TWindow;

Begin
  getX11Window := DefaultRootWindow(m_disp);
End;
