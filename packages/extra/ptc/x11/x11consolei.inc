Constructor TX11Console.Create;

Var
  s : AnsiString;

Begin
  x11disp := Nil;
  m_flags := [];
  FillChar(m_modes, SizeOf(m_modes), 0);
  m_title := '';
  
  m_modes[0] := TPTCMode.Create;
  
  configure('/usr/share/ptcpas/ptcpas.conf');
  s := fpgetenv('HOME');
  If s = '' Then
    s := '/';
  If s[Length(s)] <> '/' Then
    s := s + '/';
  s := s + '.ptcpas.conf';
  configure(s);
End;

Destructor TX11Console.Destroy;

Var
  I : Integer;

Begin
  close;
  m_title := '';
  FreeAndNil(x11disp);
  For I := Low(m_modes) To High(m_modes) Do
    FreeAndNil(m_modes[I]);
  Inherited Destroy;
End;

Procedure TX11Console.configure(Const _file : String);

Var
  F : Text;
  S : String;

Begin
  ASSignFile(F, _file);
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
    option(S);
  End;
  CloseFile(F);
End;

Function TX11Console.option(Const _option : String) : Boolean;

Begin
  option := True;
  If _option = 'default output' Then
  Begin
    { default is windowed for now }
    m_flags := m_flags - [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If _option = 'windowed output' Then
  Begin
    m_flags := m_flags - [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If _option = 'fullscreen output' Then
  Begin
    m_flags := m_flags + [PTC_X11_FULLSCREEN];
    Exit;
  End;
  If _option = 'leave window open' Then
  Begin
    m_flags := m_flags + [PTC_X11_LEAVE_WINDOW];
    Exit;
  End;
  If _option = 'leave display open' Then
  Begin
    m_flags := m_flags + [PTC_X11_LEAVE_DISPLAY];
    Exit;
  End;
  If _option = 'dga pedantic init' Then
  Begin
    m_flags := m_flags + [PTC_X11_PEDANTIC_DGA, PTC_X11_TRY_DGA];
    Exit;
  End;
  If _option = 'dga' Then
  Begin
    m_flags := m_flags + [PTC_X11_TRY_DGA];
    Exit;
  End;
  If _option = 'dga off' Then
  Begin
    m_flags := m_flags - [PTC_X11_TRY_DGA];
    Exit;
  End;
  If _option = 'default cursor' Then
  Begin
    m_flags := m_flags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE, PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    Exit;
  End;
  If _option = 'show cursor' Then
  Begin
    m_flags := (m_flags - [PTC_X11_WINDOWED_CURSOR_INVISIBLE]) + [PTC_X11_FULLSCREEN_CURSOR_VISIBLE];
    UpdateCursor;
    Exit;
  End;
  If _option = 'hide cursor' Then
  Begin
    m_flags := (m_flags - [PTC_X11_FULLSCREEN_CURSOR_VISIBLE]) + [PTC_X11_WINDOWED_CURSOR_INVISIBLE];
    UpdateCursor;
    Exit;
  End;
  If x11disp <> Nil Then
    option := x11disp.m_copy.option(_option)
  Else
    option := False;
End;

Function TX11Console.modes : PPTCMode;

Begin
  modes := @m_modes;
End;

{TODO: Find current pixel depth}
Procedure TX11Console.open(Const _title : String; _pages : Integer);

Var
  tmp : TPTCFormat;

Begin
  setTitle(_title);
  tmp := TPTCFormat.Create(32, $FF0000, $FF00, $FF);
  Try
    open(_title, tmp, _pages);
  Finally
    tmp.Free;
  End;
End;

Procedure TX11Console.open(Const _title : String; Const _format : TPTCFormat;
		           _pages : Integer);

Begin
  setTitle(_title);
  open(_title, 640, 480, _format, _pages);
End;

Procedure TX11Console.open(Const _title : String; _width, _height : Integer;
		           Const _format : TPTCFormat; _pages : Integer);

Var
  disp : PDisplay;
  screen : Integer;

Begin
  close;
  setTitle(_title);
  
  { Check if we can open an X display }
  disp := XOpenDisplay(Nil);
  If disp = Nil Then
    Raise TPTCError.Create('Cannot open X display');
  
  { DefaultScreen should be fine }
  screen := DefaultScreen(disp);
  
  FreeAndNil(x11disp);
  
  {ifndef HAVE_DGA}
  
  If (PTC_X11_TRY_DGA In m_flags) Then
  Begin
    Try
      x11disp := TX11DGADisplay.Create;
      x11disp.flags(m_flags + [PTC_X11_LEAVE_DISPLAY]);
      x11disp.open(_title, _width, _height, _format, disp, screen);
      x11disp.flags(m_flags);
    Except
      FreeAndNil(x11disp);
    End;
  End;
  
  If x11disp = Nil Then
  Begin
    x11disp := TX11WindowDisplay.Create;
    x11disp.flags(m_flags);
    x11disp.open(_title, _width, _height, _format, disp, screen);
  End;
  
  UpdateCursor;
End;

Procedure TX11Console.open(Const _title : String; Const _mode : TPTCMode;
		           _pages : Integer);

Begin
  setTitle(_title);
End;

Procedure TX11Console.close;

Begin
  FreeAndNil(x11disp);
End;

Procedure TX11Console.flush;

Begin
  update;
End;

Procedure TX11Console.finish;

Begin
  update;
End;

Procedure TX11Console.update;

Begin
  x11disp.update;
End;

Procedure TX11Console.update(Const _area : TPTCArea);

Begin
  x11disp.update(_area);
End;

Function TX11Console.NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean;

Begin
  Result := x11disp.NextEvent(event, wait, EventMask);
End;

Function TX11Console.PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent;

Begin
  Result := x11disp.PeekEvent(wait, EventMask);
End;

Procedure TX11Console.copy(Var surface : TPTCBaseSurface);

Begin
  {todo!...}
End;

Procedure TX11Console.copy(Var surface : TPTCBaseSurface;
		           Const source, destination : TPTCArea);

Begin
  {todo!...}
End;

Function TX11Console.lock : Pointer;

Begin
  lock := x11disp.lock;
End;

Procedure TX11Console.unlock;

Begin
  x11disp.unlock;
End;

Procedure TX11Console.load(Const pixels : Pointer;
		           _width, _height, _pitch : Integer;
		           Const _format : TPTCFormat;
		           Const _palette : TPTCPalette);

Begin
  x11disp.load(pixels, _width, _height, _pitch, _format, _palette);
End;

Procedure TX11Console.load(Const pixels : Pointer;
		           _width, _height, _pitch : Integer;
		           Const _format : TPTCFormat;
		           Const _palette : TPTCPalette;
		           Const source, destination : TPTCArea);

Begin
  x11disp.load(pixels, _width, _height, _pitch, _format, _palette, source, destination);
End;

Procedure TX11Console.save(pixels : Pointer;
		           _width, _height, _pitch : Integer;
		           Const _format : TPTCFormat;
		           Const _palette : TPTCPalette);

Begin
  {todo!...}
End;

Procedure TX11Console.save(pixels : Pointer;
		           _width, _height, _pitch : Integer;
		           Const _format : TPTCFormat;
		           Const _palette : TPTCPalette;
		           Const source, destination : TPTCArea);

Begin
  {todo!...}
End;

Procedure TX11Console.clear;

Var
  tmp : TPTCColor;

Begin
  If format.direct Then
    tmp := TPTCColor.Create(0, 0, 0, 0)
  Else
    tmp := TPTCColor.Create(0);
  Try
    clear(tmp);
  Finally
    tmp.Free;
  End;
End;

Procedure TX11Console.clear(Const color : TPTCColor);

Begin
  x11disp.clear(color);
End;

Procedure TX11Console.clear(Const color : TPTCColor;
		            Const _area : TPTCArea);

Begin
  x11disp.clear(color, _area);
End;

Procedure TX11Console.palette(Const _palette : TPTCPalette);

Begin
  x11disp.palette(_palette);
End;

Function TX11Console.palette : TPTCPalette;

Begin
  palette := x11disp.palette;
End;

Procedure TX11Console.clip(Const _area : TPTCArea);

Begin
  x11disp.clip(_area);
End;

Function TX11Console.width : Integer;

Begin
  width := x11disp.width;
End;

Function TX11Console.height : Integer;

Begin
  height := x11disp.height;
End;

Function TX11Console.pitch : Integer;

Begin
  pitch := x11disp.pitch;
End;

Function TX11Console.pages : Integer;

Begin
  pages := 1;
End;

Function TX11Console.area : TPTCArea;

Begin
  area := x11disp.area;
End;

Function TX11Console.clip : TPTCArea;

Begin
  clip := x11disp.clip;
End;

Function TX11Console.format : TPTCFormat;

Begin
  format := x11disp.format;
End;

Function TX11Console.name : String;

Begin
  name := 'X11';
End;

Function TX11Console.title : String;

Begin
  title := m_title;
End;

Function TX11Console.information : String;

Var
  s : String;

Begin
  If x11disp = Nil Then
    Exit('PTC X11');
  information := 'PTC X11, ';
  If x11disp.isFullScreen Then
    information := information + 'fullscreen '
  Else
    information := information + 'windowed ';
  If x11disp Is TX11WindowDisplay Then
  Begin
    If TX11WindowDisplay(x11disp).m_primary <> Nil Then
    Begin
    {$IFDEF HAVE_X11_EXTENSIONS_XSHM}
      If TX11WindowDisplay(x11disp).m_primary Is TX11SHMImage Then
        information := information + '(MIT-Shm) '
      Else
    {$ENDIF HAVE_X11_EXTENSIONS_XSHM}
        information := information + '(XImage) ';
    End
    Else
      information := information + '';
  End
  Else
    information := information + '(DGA) ';
  information := information + 'mode, ';
  Str(x11disp.width, s);
  information := information + s + 'x';
  Str(x11disp.height, s);
  information := information + s + ', ';
  Str(x11disp.format.bits, s);
  information := information + s + ' bit';
End;

Procedure TX11Console.UpdateCursor;

Begin
  If Assigned(x11disp) Then
  Begin
    If x11disp.isFullScreen Then
      x11disp.SetCursor(PTC_X11_FULLSCREEN_CURSOR_VISIBLE In m_flags)
    Else
      x11disp.SetCursor(Not (PTC_X11_WINDOWED_CURSOR_INVISIBLE In m_flags));
  End;
End;

Procedure TX11Console.setTitle(_title : String);

Begin
  m_title := _title;
End;
