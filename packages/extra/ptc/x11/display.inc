{$INCLUDE xunikey.inc}

Constructor TX11Display.Create;

Begin
  m_disp := Nil;
  m_colours := Nil;
  m_cmap := 0;
  m_flags := 0;
  m_width := 0;
  m_height := 0;
  m_functionkeys := Nil;
  m_normalkeys := Nil;
  m_copy := Nil;
  m_clear := Nil;
  m_palette := Nil;
  m_clip := Nil;
  m_area := Nil;
  m_format := Nil;
  
  m_copy := TPTCCopy.Create;
  m_clear := TPTCClear.Create;
  m_palette := TPTCPalette.Create;
  m_clip := TPTCArea.Create;
  m_area := TPTCArea.Create;
  m_format := TPTCFormat.Create;
  
  setKeyMapping;
End;

Destructor TX11Display.Destroy;

Begin
//  Writeln('lll1');
  { Just close the display, everything else is done by the subclasses }
  If (m_disp <> Nil) And ((m_flags And PTC_X11_LEAVE_DISPLAY) = 0) Then
  Begin
//    Writeln('lalalalala1');
    XFlush(m_disp);
//    Writeln('lalalalala2');
    XCloseDisplay(m_disp);
//    Writeln('lalalalala3');
    m_disp := Nil;
//    Writeln('lalalalala4');
  End;
//  Writeln('lll2');
  FreeMemAndNil(m_normalkeys);
  FreeMemAndNil(m_functionkeys);
//  Writeln('lll3');
  
  m_copy.Free;
  m_clear.Free;
  m_palette.Free;
  m_clip.Free;
  m_area.Free;
  m_format.Free;
  
  Inherited Destroy;
End;

Procedure TX11Display.load(Const pixels : Pointer; _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat; Const _palette : TPTCPalette);
Var
  Area_ : TPTCArea;
  console_pixels : Pointer;

Begin
  If clip.Equals(area) Then
  Begin
    console_pixels := lock;
    Try
      Try
	m_copy.request(_format, format);
	m_copy.palette(_palette, palette);
	m_copy.copy(pixels, 0, 0, _width, _height, _pitch, console_pixels, 0, 0,
		    width, height, pitch);
      Except
	On error : TPTCError Do
	Begin
	  Raise TPTCError.Create('failed to load pixels to console', error);
	End;
      End;
    Finally
      unlock;
    End;
  End
  Else
  Begin
    Area_ := TPTCArea.Create(0, 0, width, height);
    Try
      load(pixels, _width, _height, _pitch, _format, _palette, Area_, area);
    Finally
      Area_.Free;
    End;
  End;
End;

Procedure TX11Display.load(Const pixels : Pointer; _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat; Const _palette : TPTCPalette;
                           Const source, destination : TPTCArea);
Var
  console_pixels : Pointer;
  clipped_source, clipped_destination : TPTCArea;
  tmp : TPTCArea;

Begin
  clipped_destination := Nil;
  clipped_source := TPTCArea.Create;
  Try
    clipped_destination := TPTCArea.Create;
    console_pixels := lock;
    Try
      Try
	tmp := TPTCArea.Create(0, 0, _width, _height);
	Try
	  TPTCClipper.clip(source, tmp, clipped_source, destination, clip, clipped_destination);
	Finally
	  tmp.Free;
	End;
	m_copy.request(_format, format);
	m_copy.palette(_palette, palette);
	m_copy.copy(pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, _pitch,
		    console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, pitch);
      Except
	On error:TPTCError Do
	Begin
	  Raise TPTCError.Create('failed to load pixels to console area', error);
	End;
      End;
    Finally
      unlock;
    End;
  Finally
    clipped_source.Free;
    clipped_destination.Free;
  End;
End;

Procedure TX11Display.save(pixels : Pointer; _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat; Const _palette : TPTCPalette);

Begin
End;

Procedure TX11Display.save(pixels : Pointer; _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat; Const _palette : TPTCPalette;
		           Const source, destination : TPTCArea);

Begin
End;

Procedure TX11Display.clear(Const color : TPTCColor);

Begin
End;

Procedure TX11Display.clear(Const color : TPTCColor; Const _area : TPTCArea);

Begin
End;

Function TX11Display.palette : TPTCPalette;

Begin
  {...}
  palette := m_palette;
End;

Procedure TX11Display.clip(Const _area : TPTCArea);

Begin
  m_clip.ASSign(_area);
End;

Function TX11Display.width : Integer;

Begin
  width := m_width;
End;

Function TX11Display.height : Integer;

Begin
  height := m_height;
End;

Function TX11Display.clip : TPTCArea;

Begin
  clip := m_clip;
End;

Function TX11Display.area : TPTCArea;

Var
  tmp : TPTCArea;

Begin
  tmp := TPTCArea.Create(0, 0, m_width, m_height);
  Try
    m_area.ASSign(tmp);
  Finally
    tmp.Free;
  End;
  area := m_area;
End;

Function TX11Display.format : TPTCFormat;

Begin
  format := m_format;
End;

Procedure TX11Display.flags(_flags : LongInt);

Begin
  m_flags := _flags;
End;

Function TX11Display.getX11Display : PDisplay;

Begin
  getX11Display := m_disp;
End;

Function TX11Display.getX11Screen : Integer;

Begin
  getX11Screen := m_screen;
End;

Function TX11Display.getFormat(Const _format : TPTCFormat) : TPTCFormat;

Var
  tmp_depth : Integer;
  numfound : Integer;
  i : Integer;
  pfv : PXPixmapFormatValues;

Begin
  getFormat := Nil;
  
  { Check if our screen has the same format available. I hate how X }
  { keeps bits_per_pixel and depth different }
  tmp_depth := (PXPrivDisplay(m_disp))^.screens[m_screen].root_depth;
  
  pfv := XListPixmapFormats(m_disp, @numfound);
  Try
    For i := 0 To numfound - 1 Do
    Begin
      If pfv[i].depth = tmp_depth Then
      Begin
        tmp_depth := pfv[i].bits_per_pixel;
      End;
    End;
  Finally
    XFree(pfv);
  End;
  
  If (tmp_depth = 8) And _format.indexed Then
    getFormat := TPTCFormat.Create(8)
  Else
    If (tmp_depth = 8) And _format.direct Then
      getFormat := TPTCFormat.Create(8, $E0, $1C, $03)
    Else
      getFormat := TPTCFormat.Create(tmp_depth,
                                     (PXPrivDisplay(m_disp))^.screens[m_screen].root_visual^.red_mask,
				     (PXPrivDisplay(m_disp))^.screens[m_screen].root_visual^.green_mask,
				     (PXPrivDisplay(m_disp))^.screens[m_screen].root_visual^.blue_mask);
End;

Procedure TX11Display.setKeyMapping;

Var
  _I : Integer;

Begin
  FreeMemAndNil(m_functionkeys);
  FreeMemAndNil(m_normalkeys);
  m_functionkeys := GetMem(256 * SizeOf(Integer));
  m_normalkeys := GetMem(256 * SizeOf(Integer));
  
  For _I := 0 To 255 Do
  Begin
    m_functionkeys[_I] := Integer(PTCKEY_UNDEFINED);
    m_normalkeys[_I] := Integer(PTCKEY_UNDEFINED);
  End;
  
  { Assign function key indices from X definitions }
  m_functionkeys[$FF And XK_BackSpace] := Integer(PTCKEY_BACKSPACE);
  m_functionkeys[$FF And XK_Tab] := Integer(PTCKEY_TAB);
  m_functionkeys[$FF And XK_Clear] := Integer(PTCKEY_CLEAR);
  m_functionkeys[$FF And XK_Return] := Integer(PTCKEY_ENTER);
  m_functionkeys[$FF And XK_Pause] := Integer(PTCKEY_PAUSE);
  m_functionkeys[$FF And XK_Scroll_Lock] := Integer(PTCKEY_SCROLLLOCK);
  m_functionkeys[$FF And XK_Escape] := Integer(PTCKEY_ESCAPE);
  m_functionkeys[$FF And XK_Delete] := Integer(PTCKEY_DELETE);

  m_functionkeys[$FF And XK_Kanji] := Integer(PTCKEY_KANJI);
  
  m_functionkeys[$FF And XK_Home] := Integer(PTCKEY_HOME);
  m_functionkeys[$FF And XK_Left] := Integer(PTCKEY_LEFT);
  m_functionkeys[$FF And XK_Up] := Integer(PTCKEY_UP);
  m_functionkeys[$FF And XK_Right] := Integer(PTCKEY_RIGHT);
  m_functionkeys[$FF And XK_Down] := Integer(PTCKEY_DOWN);
  m_functionkeys[$FF And XK_Page_Up] := Integer(PTCKEY_PAGEUP);
  m_functionkeys[$FF And XK_Page_Down] := Integer(PTCKEY_PAGEDOWN);
  m_functionkeys[$FF And XK_End] := Integer(PTCKEY_END);
  
  m_functionkeys[$FF And XK_Print] := Integer(PTCKEY_PRINTSCREEN);
  m_functionkeys[$FF And XK_Insert] := Integer(PTCKEY_INSERT);
  m_functionkeys[$FF And XK_Num_Lock] := Integer(PTCKEY_NUMLOCK);

  m_functionkeys[$FF And XK_KP_0] := Integer(PTCKEY_NUMPAD0);
  m_functionkeys[$FF And XK_KP_1] := Integer(PTCKEY_NUMPAD1);
  m_functionkeys[$FF And XK_KP_2] := Integer(PTCKEY_NUMPAD2);
  m_functionkeys[$FF And XK_KP_3] := Integer(PTCKEY_NUMPAD3);
  m_functionkeys[$FF And XK_KP_4] := Integer(PTCKEY_NUMPAD4);
  m_functionkeys[$FF And XK_KP_5] := Integer(PTCKEY_NUMPAD5);
  m_functionkeys[$FF And XK_KP_6] := Integer(PTCKEY_NUMPAD6);
  m_functionkeys[$FF And XK_KP_7] := Integer(PTCKEY_NUMPAD7);
  m_functionkeys[$FF And XK_KP_8] := Integer(PTCKEY_NUMPAD8);
  m_functionkeys[$FF And XK_KP_9] := Integer(PTCKEY_NUMPAD9);

  m_functionkeys[$FF And XK_F1] := Integer(PTCKEY_F1);
  m_functionkeys[$FF And XK_F2] := Integer(PTCKEY_F2);
  m_functionkeys[$FF And XK_F3] := Integer(PTCKEY_F3);
  m_functionkeys[$FF And XK_F4] := Integer(PTCKEY_F4);
  m_functionkeys[$FF And XK_F5] := Integer(PTCKEY_F5);
  m_functionkeys[$FF And XK_F6] := Integer(PTCKEY_F6);
  m_functionkeys[$FF And XK_F7] := Integer(PTCKEY_F7);
  m_functionkeys[$FF And XK_F8] := Integer(PTCKEY_F8);
  m_functionkeys[$FF And XK_F9] := Integer(PTCKEY_F9);
  m_functionkeys[$FF And XK_F10] := Integer(PTCKEY_F10);
  m_functionkeys[$FF And XK_F11] := Integer(PTCKEY_F11);
  m_functionkeys[$FF And XK_F12] := Integer(PTCKEY_F12);

  m_functionkeys[$FF And XK_Shift_L] := Integer(PTCKEY_SHIFT);
  m_functionkeys[$FF And XK_Shift_R] := Integer(PTCKEY_SHIFT);
  m_functionkeys[$FF And XK_Control_L] := Integer(PTCKEY_CONTROL);
  m_functionkeys[$FF And XK_Control_R] := Integer(PTCKEY_CONTROL);
  m_functionkeys[$FF And XK_Caps_Lock] := Integer(PTCKEY_CAPSLOCK);
  m_functionkeys[$FF And XK_Meta_L] := Integer(PTCKEY_META);
  m_functionkeys[$FF And XK_Meta_R] := Integer(PTCKEY_META);
  m_functionkeys[$FF And XK_Alt_L] := Integer(PTCKEY_ALT);
  m_functionkeys[$FF And XK_Alt_R] := Integer(PTCKEY_ALT);

  { Assign normal key indices }
  m_normalkeys[$FF And XK_space] := Integer(PTCKEY_SPACE);
  m_normalkeys[$FF And XK_comma] := Integer(PTCKEY_COMMA);
  m_normalkeys[$FF And XK_minus] := Integer(PTCKEY_SUBTRACT);
  m_normalkeys[$FF And XK_period] := Integer(PTCKEY_PERIOD);
  m_normalkeys[$FF And XK_slash] := Integer(PTCKEY_SLASH);
  m_normalkeys[$FF And XK_0] := Integer(PTCKEY_ZERO);
  m_normalkeys[$FF And XK_1] := Integer(PTCKEY_ONE);
  m_normalkeys[$FF And XK_2] := Integer(PTCKEY_TWO);
  m_normalkeys[$FF And XK_3] := Integer(PTCKEY_THREE);
  m_normalkeys[$FF And XK_4] := Integer(PTCKEY_FOUR);
  m_normalkeys[$FF And XK_5] := Integer(PTCKEY_FIVE);
  m_normalkeys[$FF And XK_6] := Integer(PTCKEY_SIX);
  m_normalkeys[$FF And XK_7] := Integer(PTCKEY_SEVEN);
  m_normalkeys[$FF And XK_8] := Integer(PTCKEY_EIGHT);
  m_normalkeys[$FF And XK_9] := Integer(PTCKEY_NINE);
  m_normalkeys[$FF And XK_semicolon] := Integer(PTCKEY_SEMICOLON);
  m_normalkeys[$FF And XK_equal] := Integer(PTCKEY_EQUALS);

  m_normalkeys[$FF And XK_bracketleft] := Integer(PTCKEY_OPENBRACKET);
  m_normalkeys[$FF And XK_backslash] := Integer(PTCKEY_BACKSLASH);
  m_normalkeys[$FF And XK_bracketright] := Integer(PTCKEY_CLOSEBRACKET);

  m_normalkeys[$FF And XK_a] := Integer(PTCKEY_A);
  m_normalkeys[$FF And XK_b] := Integer(PTCKEY_B);
  m_normalkeys[$FF And XK_c] := Integer(PTCKEY_C);
  m_normalkeys[$FF And XK_d] := Integer(PTCKEY_D);
  m_normalkeys[$FF And XK_e] := Integer(PTCKEY_E);
  m_normalkeys[$FF And XK_f] := Integer(PTCKEY_F);
  m_normalkeys[$FF And XK_g] := Integer(PTCKEY_G);
  m_normalkeys[$FF And XK_h] := Integer(PTCKEY_H);
  m_normalkeys[$FF And XK_i] := Integer(PTCKEY_I);
  m_normalkeys[$FF And XK_j] := Integer(PTCKEY_J);
  m_normalkeys[$FF And XK_k] := Integer(PTCKEY_K);
  m_normalkeys[$FF And XK_l] := Integer(PTCKEY_L);
  m_normalkeys[$FF And XK_m] := Integer(PTCKEY_M);
  m_normalkeys[$FF And XK_n] := Integer(PTCKEY_N);
  m_normalkeys[$FF And XK_o] := Integer(PTCKEY_O);
  m_normalkeys[$FF And XK_p] := Integer(PTCKEY_P);
  m_normalkeys[$FF And XK_q] := Integer(PTCKEY_Q);
  m_normalkeys[$FF And XK_r] := Integer(PTCKEY_R);
  m_normalkeys[$FF And XK_s] := Integer(PTCKEY_S);
  m_normalkeys[$FF And XK_t] := Integer(PTCKEY_T);
  m_normalkeys[$FF And XK_u] := Integer(PTCKEY_U);
  m_normalkeys[$FF And XK_v] := Integer(PTCKEY_V);
  m_normalkeys[$FF And XK_w] := Integer(PTCKEY_W);
  m_normalkeys[$FF And XK_x] := Integer(PTCKEY_X);
  m_normalkeys[$FF And XK_y] := Integer(PTCKEY_Y);
  m_normalkeys[$FF And XK_z] := Integer(PTCKEY_Z);
End;
