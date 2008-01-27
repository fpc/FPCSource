Constructor TX11Console.Create;

Var
  s : AnsiString;

Begin
  Inherited Create;

  { default flags }
  FFlags := [PTC_X11_TRY_XSHM, PTC_X11_TRY_XF86VIDMODE];

  FTitle := '';

  Configure('/usr/share/ptcpas/ptcpas.conf');
  s := fpgetenv('HOME');
  If s = '' Then
    s := '/';
  If s[Length(s)] <> '/' Then
    s := s + '/';
  s := s + '.ptcpas.conf';
  Configure(s);
End;

Destructor TX11Console.Destroy;

Var
  I : Integer;

Begin
  Close;
  FreeAndNil(FX11Display);
  For I := Low(FModes) To High(FModes) Do
    FreeAndNil(FModes[I]);
  Inherited Destroy;
End;

Procedure TX11Console.Configure(Const AFileName : String);

Var
  F : Text;
  S : String;

Begin
  AssignFile(F, AFileName);
  {$I-}
  Reset(F);
  {$I+}
  If IOResult <> 0 Then
    Exit;
  While Not EoF(F) Do
  Begin
    {$I-}
    Readln(F, S);
    {$I+}
    If IOResult <> 0 Then
      Break;
    Option(S);
  End;
  CloseFile(F);
End;

Function TX11Console.Option(Const AOption : String) : Boolean;

Begin
  Result := True;
  If AOption = 'default output' Then
  Begin
    { default is windowed for now }
    FFlags := FFlags - [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If AOption = 'windowed output' Then
  Begin
    FFlags := FFlags - [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If AOption = 'fullscreen output' Then
  Begin
    FFlags := FFlags + [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If AOption = 'leave window open' Then
  Begin
    FFlags := FFlags + [PTC_X11_LEAVE_WINDOW];
    Exit;
  End;
  If AOption = 'leave display open' Then
  Begin
    FFlags := FFlags + [PTC_X11_LEAVE_DISPLAY];
    Exit;
  End;
  If AOption = 'dga' Then
  Begin
    FFlags := FFlags + [PTC_X11_TRY_DGA];
    Exit;
  End;
  If AOption = 'dga off' Then
  Begin
    FFlags := FFlags - [PTC_X11_TRY_DGA];
    Exit;
  End;
  If AOption = 'xf86vidmode' Then
  Begin
    FFlags := FFlags + [PTC_X11_TRY_XF86VIDMODE];
    Exit;
  End;
  If AOption = 'xf86vidmode off' Then
  Begin
    FFlags := FFlags - [PTC_X11_TRY_XF86VIDMODE];
    Exit;
  End;
  If AOption = 'xrandr' Then
  Begin
    FFlags := FFlags + [PTC_X11_TRY_XRANDR];
    Exit;
  End;
  If AOption = 'xrandr off' Then
  Begin
    FFlags := FFlags - [PTC_X11_TRY_XRANDR];
    Exit;
  End;
  If AOption = 'xshm' Then
  Begin
    FFlags := FFlags + [PTC_X11_TRY_XSHM];
    Exit;
  End;
  If AOption = 'xshm off' Then
  Begin
    FFlags := FFlags - [PTC_X11_TRY_XSHM];
    Exit;
  End;
  If AOption = 'default cursor' Then
  Begin
    FFlags := FFlags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE, PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    Exit;
  End;
  If AOption = 'show cursor' Then
  Begin
    FFlags := (FFlags - [PTC_X11_WINDOWED_CURSOR_INVISIBLE]) + [PTC_X11_FULLSCREEN_CURSOR_VISIBLE];
    UpdateCursor;
    Exit;
  End;
  If AOption = 'hide cursor' Then
  Begin
    FFlags := (FFlags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE]) + [PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    Exit;
  End;
  If AOption = 'enable logging' Then
  Begin
    LOG_enabled := True;
    Result := True;
    Exit;
  End;
  If AOption = 'disable logging' Then
  Begin
    LOG_enabled := False;
    Result := True;
    Exit;
  End;

  If Assigned(FX11Display) Then
    Result := FX11Display.FCopy.Option(AOption)
  Else
    Result := False;
End;

Function TX11Console.Modes : PPTCMode;

Var
  I : Integer;

Begin
  For I := Low(FModes) To High(FModes) Do
    FreeAndNil(FModes[I]);

  If FX11Display = Nil Then
    FX11Display := CreateDisplay;

  FX11Display.GetModes(FModes);

  Result := @FModes[0];
End;

{TODO: Find current pixel depth}
Procedure TX11Console.Open(Const ATitle : String; APages : Integer = 0);

Var
  tmp : TPTCFormat;

Begin
  tmp := TPTCFormat.Create(32, $FF0000, $FF00, $FF);
  Try
    Open(ATitle, tmp, APages);
  Finally
    tmp.Free;
  End;
End;

Procedure TX11Console.Open(Const ATitle : String; Const AFormat : TPTCFormat;
                           APages : Integer = 0);

Begin
  Open(ATitle, 640, 480, AFormat, APages);
End;

Procedure TX11Console.Open(Const ATitle : String; Const AMode : TPTCMode;
                           APages : Integer = 0);

Begin
  Open(ATitle, AMode.Width, AMode.Height, AMode.Format, APages);
End;

Function TX11Console.CreateDisplay : TX11Display;

Var
  display : PDisplay;
  screen : Integer;

Begin
  { Check if we can open an X display }
  display := XOpenDisplay(Nil);
  If display = Nil Then
    Raise TPTCError.Create('Cannot open X display');

  { DefaultScreen should be fine }
  screen := DefaultScreen(display);

  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}
  If PTC_X11_TRY_DGA In FFlags Then
  Begin
    Try
      Result := TX11DGA2Display.Create(display, screen, FFlags + [PTC_X11_LEAVE_DISPLAY]);
      Result.SetFlags(FFlags);
      Exit;
    Except
      LOG('DGA 2.0 failed');
    End;
  End;
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}

  {$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}
  If PTC_X11_TRY_DGA In FFlags Then
  Begin
    Try
      Result := TX11DGA1Display.Create(display, screen, FFlags + [PTC_X11_LEAVE_DISPLAY]);
      Result.SetFlags(FFlags);
    Except
      LOG('DGA 1.0 failed');
    End;
  End;
  {$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}

  Result := TX11WindowDisplay.Create(display, screen, FFlags);
End;

Procedure TX11Console.Open(Const ATitle : String; AWidth, AHeight : Integer;
                           Const AFormat : TPTCFormat; APages : Integer = 0);

Begin
  Close;
  FTitle := ATitle;

  If FX11Display = Nil Then
    FX11Display := CreateDisplay;
  FX11Display.Open(ATitle, AWidth, AHeight, AFormat);

  UpdateCursor;
End;

Procedure TX11Console.Close;

Begin
  FreeAndNil(FX11Display);
End;

Procedure TX11Console.Flush;

Begin
  Update;
End;

Procedure TX11Console.Finish;

Begin
  Update;
End;

Procedure TX11Console.Update;

Begin
  FX11Display.Update;
End;

Procedure TX11Console.Update(Const AArea : TPTCArea);

Begin
  FX11Display.Update(AArea);
End;

Function TX11Console.NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean;

Begin
  Result := FX11Display.NextEvent(AEvent, AWait, AEventMask);
End;

Function TX11Console.PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent;

Begin
  Result := FX11Display.PeekEvent(AWait, AEventMask);
End;

Procedure TX11Console.Copy(Var ASurface : TPTCBaseSurface);

Begin
  {todo!...}
End;

Procedure TX11Console.Copy(Var ASurface : TPTCBaseSurface;
                           Const ASource, ADestination : TPTCArea);

Begin
  {todo!...}
End;

Function TX11Console.Lock : Pointer;

Begin
  Result := FX11Display.Lock;
End;

Procedure TX11Console.Unlock;

Begin
  FX11Display.Unlock;
End;

Procedure TX11Console.Load(Const APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette);

Begin
  FX11Display.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette);
End;

Procedure TX11Console.Load(Const APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);

Begin
  FX11Display.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, ASource, ADestination);
End;

Procedure TX11Console.Save(APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette);

Begin
  {todo!...}
End;

Procedure TX11Console.Save(APixels : Pointer;
                           AWidth, AHeight, APitch : Integer;
                           Const AFormat : TPTCFormat;
                           Const APalette : TPTCPalette;
                           Const ASource, ADestination : TPTCArea);

Begin
  {todo!...}
End;

Procedure TX11Console.Clear;

Var
  tmp : TPTCColor;

Begin
  If Format.Direct Then
    tmp := TPTCColor.Create(0, 0, 0, 0)
  Else
    tmp := TPTCColor.Create(0);
  Try
    Clear(tmp);
  Finally
    tmp.Free;
  End;
End;

Procedure TX11Console.Clear(Const AColor : TPTCColor);

Begin
  FX11Display.Clear(AColor);
End;

Procedure TX11Console.Clear(Const AColor : TPTCColor;
                            Const AArea : TPTCArea);

Begin
  FX11Display.Clear(AColor, AArea);
End;

Procedure TX11Console.Palette(Const APalette : TPTCPalette);

Begin
  FX11Display.Palette(APalette);
End;

Function TX11Console.Palette : TPTCPalette;

Begin
  Result := FX11Display.Palette;
End;

Procedure TX11Console.Clip(Const AArea : TPTCArea);

Begin
  FX11Display.Clip(AArea);
End;

Function TX11Console.GetWidth : Integer;

Begin
  Result := FX11Display.Width;
End;

Function TX11Console.GetHeight : Integer;

Begin
  Result := FX11Display.Height;
End;

Function TX11Console.GetPitch : Integer;

Begin
  Result := FX11Display.Pitch;
End;

Function TX11Console.GetPages : Integer;

Begin
  Result := 2;
End;

Function TX11Console.GetArea : TPTCArea;

Begin
  Result := FX11Display.Area;
End;

Function TX11Console.Clip : TPTCArea;

Begin
  Result := FX11Display.Clip;
End;

Function TX11Console.GetFormat : TPTCFormat;

Begin
  Result := FX11Display.Format;
End;

Function TX11Console.GetName : String;

Begin
  Result := 'X11';
End;

Function TX11Console.GetTitle : String;

Begin
  Result := FTitle;
End;

Function TX11Console.GetInformation : String;

Begin
  If FX11Display = Nil Then
    Exit('PTC X11');
  Result := 'PTC X11, ';
  If FX11Display.IsFullScreen Then
    Result := Result + 'fullscreen '
  Else
    Result := Result + 'windowed ';

  { TODO: use virtual methods, instead of "is" }
  If FX11Display Is TX11WindowDisplay Then
  Begin
    If TX11WindowDisplay(FX11Display).FPrimary <> Nil Then
      Result := Result + '(' + TX11WindowDisplay(FX11Display).FPrimary.Name + ') '
    Else
      Result := Result + '';
  End
  Else
  Begin
    {$IFDEF ENABLE_X11_EXTENSION_XF86DGA2}
    If FX11Display Is TX11DGA2Display Then
      Result := Result + '(DGA) '
    Else
    {$ENDIF ENABLE_X11_EXTENSION_XF86DGA2}
    {$IFDEF ENABLE_X11_EXTENSION_XF86DGA1}
    If FX11Display Is TX11DGA1Display Then
      Result := Result + '(DGA) '
    Else
    {$ENDIF ENABLE_X11_EXTENSION_XF86DGA1}
    Begin
      {...}
    End;
  End;
  Result := Result + 'mode, ' +
            IntToStr(FX11Display.Width) + 'x' +
            IntToStr(FX11Display.Height) + ', ' +
            IntToStr(FX11Display.Format.Bits) + ' bit';
End;

Procedure TX11Console.UpdateCursor;

Begin
  If Assigned(FX11Display) Then
  Begin
    If FX11Display.IsFullScreen Then
      FX11Display.SetCursor(PTC_X11_FULLSCREEN_CURSOR_VISIBLE In FFlags)
    Else
      FX11Display.SetCursor(Not (PTC_X11_WINDOWED_CURSOR_INVISIBLE In FFlags));
  End;
End;
