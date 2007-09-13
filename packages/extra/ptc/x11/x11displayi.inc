{$INCLUDE xunikey.inc}

Constructor TX11Display.Create(ADisplay : PDisplay; AScreen : Integer; Const AFlags : TX11Flags);

Begin
  FFlags := AFlags;

  FDisplay := ADisplay;
  FScreen := AScreen;

  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  FPalette := TPTCPalette.Create;
  FClip := TPTCArea.Create;
  FArea := TPTCArea.Create;
  FFormat := TPTCFormat.Create;
  FEventQueue := TEventQueue.Create;

  SetKeyMapping;
End;

Destructor TX11Display.Destroy;

Begin
  { Just close the display, everything else is done by the subclasses }
  If (FDisplay <> Nil) And (Not (PTC_X11_LEAVE_DISPLAY In FFlags)) Then
  Begin
    XFlush(FDisplay);
    XCloseDisplay(FDisplay);
    FDisplay := Nil;
  End;
  FreeMemAndNil(FNormalKeys);
  FreeMemAndNil(FFunctionKeys);

  FCopy.Free;
  FClear.Free;
  FPalette.Free;
  FClip.Free;
  FArea.Free;
  FFormat.Free;
  FEventQueue.Free;

  Inherited Destroy;
End;

Procedure TX11Display.Load(Const APixels : Pointer; AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat; Const APalette : TPTCPalette);
Var
  Area_ : TPTCArea;
  console_pixels : Pointer;

Begin
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

Procedure TX11Display.Load(Const APixels : Pointer; AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat; Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);
Var
  console_pixels : Pointer;
  clipped_source, clipped_destination : TPTCArea;
  tmp : TPTCArea;

Begin
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

Procedure TX11Display.Save(APixels : Pointer; AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat; Const APalette : TPTCPalette);

Begin
End;

Procedure TX11Display.Save(APixels : Pointer; AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat; Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);

Begin
End;

Procedure TX11Display.Clear(Const AColor : TPTCColor);

Begin
End;

Procedure TX11Display.Clear(Const AColor : TPTCColor; Const AArea : TPTCArea);

Begin
End;

Function TX11Display.Palette : TPTCPalette;

Begin
  Result := FPalette;
End;

Procedure TX11Display.Clip(Const AArea : TPTCArea);

Begin
  FClip.Assign(AArea);
End;

Function TX11Display.GetWidth : Integer;

Begin
  Result := FWidth;
End;

Function TX11Display.GetHeight : Integer;

Begin
  Result := FHeight;
End;

Function TX11Display.Clip : TPTCArea;

Begin
  Result := FClip;
End;

Function TX11Display.GetArea : TPTCArea;

Var
  tmp : TPTCArea;

Begin
  tmp := TPTCArea.Create(0, 0, FWidth, FHeight);
  Try
    FArea.Assign(tmp);
  Finally
    tmp.Free;
  End;
  Result := FArea;
End;

Function TX11Display.GetFormat : TPTCFormat;

Begin
  Result := FFormat;
End;

Procedure TX11Display.SetFlags(AFlags : TX11Flags);

Begin
  FFlags := AFlags;
End;

Function TX11Display.GetX11Display : PDisplay;

Begin
  Result := FDisplay;
End;

Function TX11Display.GetX11Screen : Integer;

Begin
  Result := FScreen;
End;

Function TX11Display.GetX11Format(Const AFormat : TPTCFormat) : TPTCFormat;

Var
  tmp_depth : Integer;
  numfound : Integer;
  i : Integer;
  pfv : PXPixmapFormatValues;

Begin
  Result := Nil;

  { Check if our screen has the same format available. I hate how X }
  { keeps bits_per_pixel and depth different }
  tmp_depth := DisplayPlanes(FDisplay, FScreen);

  pfv := XListPixmapFormats(FDisplay, @numfound);
  Try
    For i := 0 To numfound - 1 Do
    Begin
      If pfv[i].depth = tmp_depth Then
      Begin
        tmp_depth := pfv[i].bits_per_pixel;
        Break;
      End;
    End;
  Finally
    XFree(pfv);
  End;

  If (tmp_depth = 8) And AFormat.Indexed Then
    Result := TPTCFormat.Create(8)
  Else
    If (tmp_depth = 8) And AFormat.Direct Then
      Result := TPTCFormat.Create(8, $E0, $1C, $03)
    Else
      Result := TPTCFormat.Create(tmp_depth,
                                 DefaultVisual(FDisplay, FScreen)^.red_mask,
                                 DefaultVisual(FDisplay, FScreen)^.green_mask,
                                 DefaultVisual(FDisplay, FScreen)^.blue_mask);
End;

Procedure TX11Display.SetKeyMapping;

Var
  _I : Integer;

Begin
  FreeMemAndNil(FFunctionKeys);
  FreeMemAndNil(FNormalKeys);
  FFunctionKeys := GetMem(256 * SizeOf(Integer));
  FNormalKeys := GetMem(256 * SizeOf(Integer));

  For _I := 0 To 255 Do
  Begin
    FFunctionKeys[_I] := Integer(PTCKEY_UNDEFINED);
    FNormalKeys[_I] := Integer(PTCKEY_UNDEFINED);
  End;

  { Assign function key indices from X definitions }
  FFunctionKeys[$FF And XK_BackSpace] := Integer(PTCKEY_BACKSPACE);
  FFunctionKeys[$FF And XK_Tab] := Integer(PTCKEY_TAB);
  FFunctionKeys[$FF And XK_Clear] := Integer(PTCKEY_CLEAR);
  FFunctionKeys[$FF And XK_Return] := Integer(PTCKEY_ENTER);
  FFunctionKeys[$FF And XK_Pause] := Integer(PTCKEY_PAUSE);
  FFunctionKeys[$FF And XK_Scroll_Lock] := Integer(PTCKEY_SCROLLLOCK);
  FFunctionKeys[$FF And XK_Escape] := Integer(PTCKEY_ESCAPE);
  FFunctionKeys[$FF And XK_Delete] := Integer(PTCKEY_DELETE);

  FFunctionKeys[$FF And XK_Kanji] := Integer(PTCKEY_KANJI);

  FFunctionKeys[$FF And XK_Home] := Integer(PTCKEY_HOME);
  FFunctionKeys[$FF And XK_Left] := Integer(PTCKEY_LEFT);
  FFunctionKeys[$FF And XK_Up] := Integer(PTCKEY_UP);
  FFunctionKeys[$FF And XK_Right] := Integer(PTCKEY_RIGHT);
  FFunctionKeys[$FF And XK_Down] := Integer(PTCKEY_DOWN);
  FFunctionKeys[$FF And XK_Page_Up] := Integer(PTCKEY_PAGEUP);
  FFunctionKeys[$FF And XK_Page_Down] := Integer(PTCKEY_PAGEDOWN);
  FFunctionKeys[$FF And XK_End] := Integer(PTCKEY_END);

  FFunctionKeys[$FF And XK_Print] := Integer(PTCKEY_PRINTSCREEN);
  FFunctionKeys[$FF And XK_Insert] := Integer(PTCKEY_INSERT);
  FFunctionKeys[$FF And XK_Num_Lock] := Integer(PTCKEY_NUMLOCK);

  FFunctionKeys[$FF And XK_KP_0] := Integer(PTCKEY_NUMPAD0);
  FFunctionKeys[$FF And XK_KP_1] := Integer(PTCKEY_NUMPAD1);
  FFunctionKeys[$FF And XK_KP_2] := Integer(PTCKEY_NUMPAD2);
  FFunctionKeys[$FF And XK_KP_3] := Integer(PTCKEY_NUMPAD3);
  FFunctionKeys[$FF And XK_KP_4] := Integer(PTCKEY_NUMPAD4);
  FFunctionKeys[$FF And XK_KP_5] := Integer(PTCKEY_NUMPAD5);
  FFunctionKeys[$FF And XK_KP_6] := Integer(PTCKEY_NUMPAD6);
  FFunctionKeys[$FF And XK_KP_7] := Integer(PTCKEY_NUMPAD7);
  FFunctionKeys[$FF And XK_KP_8] := Integer(PTCKEY_NUMPAD8);
  FFunctionKeys[$FF And XK_KP_9] := Integer(PTCKEY_NUMPAD9);

  FFunctionKeys[$FF And XK_F1] := Integer(PTCKEY_F1);
  FFunctionKeys[$FF And XK_F2] := Integer(PTCKEY_F2);
  FFunctionKeys[$FF And XK_F3] := Integer(PTCKEY_F3);
  FFunctionKeys[$FF And XK_F4] := Integer(PTCKEY_F4);
  FFunctionKeys[$FF And XK_F5] := Integer(PTCKEY_F5);
  FFunctionKeys[$FF And XK_F6] := Integer(PTCKEY_F6);
  FFunctionKeys[$FF And XK_F7] := Integer(PTCKEY_F7);
  FFunctionKeys[$FF And XK_F8] := Integer(PTCKEY_F8);
  FFunctionKeys[$FF And XK_F9] := Integer(PTCKEY_F9);
  FFunctionKeys[$FF And XK_F10] := Integer(PTCKEY_F10);
  FFunctionKeys[$FF And XK_F11] := Integer(PTCKEY_F11);
  FFunctionKeys[$FF And XK_F12] := Integer(PTCKEY_F12);

  FFunctionKeys[$FF And XK_Shift_L] := Integer(PTCKEY_SHIFT);
  FFunctionKeys[$FF And XK_Shift_R] := Integer(PTCKEY_SHIFT);
  FFunctionKeys[$FF And XK_Control_L] := Integer(PTCKEY_CONTROL);
  FFunctionKeys[$FF And XK_Control_R] := Integer(PTCKEY_CONTROL);
  FFunctionKeys[$FF And XK_Caps_Lock] := Integer(PTCKEY_CAPSLOCK);
  FFunctionKeys[$FF And XK_Meta_L] := Integer(PTCKEY_META);
  FFunctionKeys[$FF And XK_Meta_R] := Integer(PTCKEY_META);
  FFunctionKeys[$FF And XK_Alt_L] := Integer(PTCKEY_ALT);
  FFunctionKeys[$FF And XK_Alt_R] := Integer(PTCKEY_ALT);

  { Assign normal key indices }
  FNormalKeys[$FF And XK_space] := Integer(PTCKEY_SPACE);
  FNormalKeys[$FF And XK_comma] := Integer(PTCKEY_COMMA);
  FNormalKeys[$FF And XK_minus] := Integer(PTCKEY_SUBTRACT);
  FNormalKeys[$FF And XK_period] := Integer(PTCKEY_PERIOD);
  FNormalKeys[$FF And XK_slash] := Integer(PTCKEY_SLASH);
  FNormalKeys[$FF And XK_0] := Integer(PTCKEY_ZERO);
  FNormalKeys[$FF And XK_1] := Integer(PTCKEY_ONE);
  FNormalKeys[$FF And XK_2] := Integer(PTCKEY_TWO);
  FNormalKeys[$FF And XK_3] := Integer(PTCKEY_THREE);
  FNormalKeys[$FF And XK_4] := Integer(PTCKEY_FOUR);
  FNormalKeys[$FF And XK_5] := Integer(PTCKEY_FIVE);
  FNormalKeys[$FF And XK_6] := Integer(PTCKEY_SIX);
  FNormalKeys[$FF And XK_7] := Integer(PTCKEY_SEVEN);
  FNormalKeys[$FF And XK_8] := Integer(PTCKEY_EIGHT);
  FNormalKeys[$FF And XK_9] := Integer(PTCKEY_NINE);
  FNormalKeys[$FF And XK_semicolon] := Integer(PTCKEY_SEMICOLON);
  FNormalKeys[$FF And XK_equal] := Integer(PTCKEY_EQUALS);

  FNormalKeys[$FF And XK_bracketleft] := Integer(PTCKEY_OPENBRACKET);
  FNormalKeys[$FF And XK_backslash] := Integer(PTCKEY_BACKSLASH);
  FNormalKeys[$FF And XK_bracketright] := Integer(PTCKEY_CLOSEBRACKET);

  FNormalKeys[$FF And XK_a] := Integer(PTCKEY_A);
  FNormalKeys[$FF And XK_b] := Integer(PTCKEY_B);
  FNormalKeys[$FF And XK_c] := Integer(PTCKEY_C);
  FNormalKeys[$FF And XK_d] := Integer(PTCKEY_D);
  FNormalKeys[$FF And XK_e] := Integer(PTCKEY_E);
  FNormalKeys[$FF And XK_f] := Integer(PTCKEY_F);
  FNormalKeys[$FF And XK_g] := Integer(PTCKEY_G);
  FNormalKeys[$FF And XK_h] := Integer(PTCKEY_H);
  FNormalKeys[$FF And XK_i] := Integer(PTCKEY_I);
  FNormalKeys[$FF And XK_j] := Integer(PTCKEY_J);
  FNormalKeys[$FF And XK_k] := Integer(PTCKEY_K);
  FNormalKeys[$FF And XK_l] := Integer(PTCKEY_L);
  FNormalKeys[$FF And XK_m] := Integer(PTCKEY_M);
  FNormalKeys[$FF And XK_n] := Integer(PTCKEY_N);
  FNormalKeys[$FF And XK_o] := Integer(PTCKEY_O);
  FNormalKeys[$FF And XK_p] := Integer(PTCKEY_P);
  FNormalKeys[$FF And XK_q] := Integer(PTCKEY_Q);
  FNormalKeys[$FF And XK_r] := Integer(PTCKEY_R);
  FNormalKeys[$FF And XK_s] := Integer(PTCKEY_S);
  FNormalKeys[$FF And XK_t] := Integer(PTCKEY_T);
  FNormalKeys[$FF And XK_u] := Integer(PTCKEY_U);
  FNormalKeys[$FF And XK_v] := Integer(PTCKEY_V);
  FNormalKeys[$FF And XK_w] := Integer(PTCKEY_W);
  FNormalKeys[$FF And XK_x] := Integer(PTCKEY_X);
  FNormalKeys[$FF And XK_y] := Integer(PTCKEY_Y);
  FNormalKeys[$FF And XK_z] := Integer(PTCKEY_Z);
End;
