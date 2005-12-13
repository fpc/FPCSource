Constructor TX11Console.Create;

Var
  s : AnsiString;

Begin
  x11disp := Nil;
  m_flags := 0;
  FillChar(m_modes, SizeOf(m_modes), 0);
  m_title := '';
  
  m_modes[0] := TPTCMode.Create;
  
  configure('/usr/share/ptc/ptc.conf');
  s := fpgetenv('HOME');
  If s = '' Then
    s := '/';
  If s[Length(s)] <> '/' Then
    s := s + '/';
  s := s + '.ptc.conf';
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
  If _option = 'dga pedantic init' Then
  Begin
    m_flags := m_flags Or PTC_X11_PEDANTIC_DGA;
    Exit;
  End;
  If _option = 'dga off' Then
  Begin
    m_flags := m_flags Or PTC_X11_NODGA;
    Exit;
  End;
  If _option = 'leave window open' Then
  Begin
    m_flags := m_flags Or PTC_X11_LEAVE_WINDOW;
    Exit;
  End;
  If _option = 'leave display open' Then
  Begin
    m_flags := m_flags Or PTC_X11_LEAVE_DISPLAY;
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
  
  If (m_flags And PTC_X11_NODGA) = 0 Then
  Begin
    Try
      x11disp := TX11DGADisplay.Create;
      x11disp.flags(m_flags Or PTC_X11_LEAVE_DISPLAY);
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

Procedure TX11Console.internal_ReadKey(k : TPTCKey);

Begin
  x11disp.internal_ReadKey(k);
End;

Function TX11Console.internal_PeekKey(k : TPTCKey) : Boolean;

Begin
  Result := x11disp.internal_PeekKey(k);
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
  If x11disp Is TX11WindowDisplay Then
  Begin
    If TX11WindowDisplay(x11disp).m_primary <> Nil Then
    Begin
    {$IFDEF HAVE_X11_EXTENSIONS_XSHM}
      If TX11WindowDisplay(x11disp).m_primary Is TX11SHMImage Then
        information := information + 'windowed (MIT-Shm) mode'
      Else
    {$ENDIF HAVE_X11_EXTENSIONS_XSHM}
        information := information + 'windowed (XImage) mode';
    End
    Else
      information := information + 'windowed mode';
  End
  Else
    information := information + 'direct graphics access (DGA) mode';
  information := information + ', ';
  Str(x11disp.width, s);
  information := information + s + 'x';
  Str(x11disp.height, s);
  information := information + s + ', ';
  Str(x11disp.format.bits, s);
  information := information + s + ' bit';
End;

Procedure TX11Console.setTitle(_title : String);

Begin
  m_title := _title;
End;
