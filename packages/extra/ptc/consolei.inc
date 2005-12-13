{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Constructor TPTCConsole.Create;

Var
  I : Integer;
  {$IFDEF UNIX}
  s : AnsiString;
  {$ENDIF UNIX}

Begin
  Inherited Create;
  console := Nil;
  hacky_option_console_flag := False;
  FillChar(m_modes, SizeOf(m_modes), 0);
  For I := Low(m_modes) To High(m_modes) Do
    m_modes[I] := TPTCMode.Create;
  {$IFDEF UNIX}
  configure('/usr/share/ptc/ptc.conf');
  s := fpgetenv('HOME');
  If s = '' Then
    s := '/';
  If s[Length(s)] <> '/' Then
    s := s + '/';
  s := s + '.ptc.conf';
  configure(s);
  {$ELSE UNIX}
  configure('ptc.cfg');
  {$ENDIF UNIX}
End;

Destructor TPTCConsole.Destroy;

Var
  I : Integer;

Begin
  close;
  console.Free;
  For I := Low(m_modes) To High(m_modes) Do
    m_modes[I].Free;
  Inherited Destroy;
End;

Procedure TPTCConsole.configure(Const _file : String);

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

Function TPTCConsole.option(Const _option : String) : Boolean;

Begin
{$IFDEF PTC_LOGGING}
  If _option = 'enable logging' Then
  Begin
    LOG_enabled := True;
    option := True;
    Exit;
  End;
  If _option = 'disable logging' Then
  Begin
    LOG_enabled := False;
    option := True;
    Exit;
  End;
{$ENDIF PTC_LOGGING}

  If Assigned(console) Then
    option := console.option(_option)
  Else
  Begin
    console := ConsoleCreate(_option);
    If Assigned(console) Then
    Begin
      hacky_option_console_flag := True;
      option := True;
    End
    Else
      option := False;
  End;
End;

Function TPTCConsole.modes : PPTCMode;

Var
  _console : TPTCBaseConsole;
  index, mode : Integer;
  local : Integer;
  _modes : PPTCMode;
  tmp : TPTCMode;

Begin
  If Assigned(console) Then
    modes := console.modes
  Else
  Begin
    _console := Nil;
    index := -1;
    mode := 0;
    Try
      Repeat
        Inc(index);
        Try
          _console := ConsoleCreate(index);
        Except
          On TPTCError Do Begin
            FreeAndNil(_console);
            Continue;
          End;
        End;
        If _console = Nil Then
          Break;
        _modes := _console.modes;
        local := 0;
        While _modes[local].valid Do
        Begin
          m_modes[mode].ASSign(_modes[local]);
          Inc(local);
          Inc(mode);
        End;
        FreeAndNil(_console);
      Until False;
    Finally
      _console.Free;
    End;
    { todo: strip duplicate modes from list? }
    tmp := TPTCMode.Create;
    Try
      m_modes[mode].ASSign(tmp);
    Finally
      tmp.Free;
    End;
    modes := m_modes;
  End;
End;

Procedure TPTCConsole.open(Const _title : String; _pages : Integer);{ Overload;}

Var
  composite, tmp : TPTCError;
  index : Integer;
  success : Boolean;

Begin
  If Assigned(console) Then
  Begin
    Try
      console.open(_title, _pages);
      Exit;
    Except
      On error : TPTCError Do Begin
        FreeAndNil(console);
        If hacky_option_console_flag Then
        Begin
          hacky_option_console_flag := False;
          Raise TPTCError.Create('could not open console', error);
        End;
      End;
    End;
  End;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  Try
    Repeat
      Inc(index);
      Try
        console := ConsoleCreate(index);
        If console = Nil Then
          Break;
        console.open(_title, _pages);
        success := True;
        Exit;
      Except
        On error : TPTCError Do Begin
          tmp := TPTCError.Create(error.message, composite);
          Try
            composite.ASSign(tmp);
          Finally
            tmp.Free;
          End;
          FreeAndNil(console);
          Continue;
        End;
      End;
    Until False;
    console := Nil;
    Raise TPTCError.Create(composite);
  Finally
    composite.Free;
    If Not success Then
      FreeAndNil(console);
  End;
End;

Procedure TPTCConsole.open(Const _title : String; Const _format : TPTCFormat;
                           _pages : Integer);{ Overload;}

Var
  composite, tmp : TPTCError;
  index : Integer;
  success : Boolean;

Begin
  If Assigned(console) Then
  Begin
    Try
      console.open(_title, _format, _pages);
      Exit;
    Except
      On error : TPTCError Do Begin
        FreeAndNil(console);
        If hacky_option_console_flag Then
        Begin
          hacky_option_console_flag := False;
          Raise TPTCError.Create('could not open console', error);
        End;
      End;
    End;
  End;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  Try
    Repeat
      Inc(index);
      Try
        console := ConsoleCreate(index);
        If console = Nil Then
          Break;
        console.open(_title, _format, _pages);
        success := True;
        Exit;
      Except
        On error : TPTCError Do Begin
          tmp := TPTCError.Create(error.message, composite);
          Try
            composite.ASSign(tmp);
          Finally
            tmp.Free;
          End;
          FreeAndNil(console);
          Continue;
        End;
      End;
    Until False;
    console := Nil;
    Raise TPTCError.Create(composite);
  Finally
    composite.Free;
    If Not success Then
      FreeAndNil(console);
  End;
End;

Procedure TPTCConsole.open(Const _title : String; _width, _height : Integer;
                           Const _format : TPTCFormat; _pages : Integer);{ Overload;}

Var
  composite, tmp : TPTCError;
  index : Integer;
  success : Boolean;

Begin
  If Assigned(console) Then
  Begin
    Try
      console.open(_title, _width, _height, _format, _pages);
      Exit;
    Except
      On error : TPTCError Do Begin
        FreeAndNil(console);
        If hacky_option_console_flag Then
        Begin
          hacky_option_console_flag := False;
          Raise TPTCError.Create('could not open console', error);
        End;
      End;
    End;
  End;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  Try
    Repeat
      Inc(index);
      Try
        console := ConsoleCreate(index);
        If console = Nil Then
          Break;
        console.open(_title, _width, _height, _format, _pages);
        success := True;
        Exit;
      Except
        On error : TPTCError Do Begin
          tmp := TPTCError.Create(error.message, composite);
          Try
            composite.ASSign(tmp);
          Finally
            tmp.Free;
          End;
          FreeAndNil(console);
          Continue;
        End;
      End;
    Until False;
    console := Nil;
    Raise TPTCError.Create(composite);
  Finally
    composite.Free;
    If Not success Then
      FreeAndNil(console);
  End;
End;

Procedure TPTCConsole.open(Const _title : String; Const _mode : TPTCMode;
                           _pages : Integer);{ Overload;}

Var
  composite, tmp : TPTCError;
  index : Integer;
  success : Boolean;

Begin
  If Assigned(console) Then
  Begin
    Try
      console.open(_title, _mode, _pages);
      Exit;
    Except
      On error : TPTCError Do Begin
        FreeAndNil(console);
        If hacky_option_console_flag Then
        Begin
          hacky_option_console_flag := False;
          Raise TPTCError.Create('could not open console', error);
        End;
      End;
    End;
  End;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  Try
    Repeat
      Inc(index);
      Try
        console := ConsoleCreate(index);
        If console = Nil Then
          Break;
        console.open(_title, _mode, _pages);
        success := True;
        Exit;
      Except
        On error : TPTCError Do Begin
          tmp := TPTCError.Create(error.message, composite);
          Try
            composite.ASSign(tmp);
          Finally
            tmp.Free;
          End;
          FreeAndNil(console);
          Continue;
        End;
      End;
    Until False;
    console := Nil;
    Raise TPTCError.Create(composite);
  Finally
    composite.Free;
    If Not success Then
      FreeAndNil(console);
  End;
End;

Procedure TPTCConsole.close;

Begin
  If Assigned(console) Then
    console.close;
  hacky_option_console_flag := False;
End;

Procedure TPTCConsole.flush;

Begin
  check;
  console.flush;
End;

Procedure TPTCConsole.finish;

Begin
  check;
  console.finish;
End;

Procedure TPTCConsole.update;

Begin
  check;
  console.update;
End;

Procedure TPTCConsole.update(Const _area : TPTCArea);

Begin
  check;
  console.update(_area);
End;

Procedure TPTCConsole.internal_ReadKey(k : TPTCKey);

Begin
  check;
  console.internal_ReadKey(k);
End;

Function TPTCConsole.internal_PeekKey(k : TPTCKey) : Boolean;

Begin
  check;
  Result := console.internal_PeekKey(k);
End;

Procedure TPTCConsole.copy(Var surface : TPTCBaseSurface);

Begin
  check;
  console.copy(surface);
End;

Procedure TPTCConsole.copy(Var surface : TPTCBaseSurface;
                           Const source, destination : TPTCArea);

Begin
  check;
  console.copy(surface, source, destination);
End;

Function TPTCConsole.lock : Pointer;

Begin
  check;
  lock := console.lock;
End;

Procedure TPTCConsole.unlock;

Begin
  check;
  console.unlock;
End;

Procedure TPTCConsole.load(Const pixels : Pointer;
                           _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat;
                           Const _palette : TPTCPalette);

Begin
  check;
  console.load(pixels, _width, _height, _pitch, _format, _palette);
End;

Procedure TPTCConsole.load(Const pixels : Pointer;
                           _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat;
                           Const _palette : TPTCPalette;
                           Const source, destination : TPTCArea);

Begin
  check;
  console.load(pixels, _width, _height, _pitch, _format, _palette,
               source, destination);
End;

Procedure TPTCConsole.save(pixels : Pointer;
                           _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat;
                           Const _palette : TPTCPalette);

Begin
  check;
  console.save(pixels, _width, _height, _pitch, _format, _palette);
End;

Procedure TPTCConsole.save(pixels : Pointer;
                           _width, _height, _pitch : Integer;
                           Const _format : TPTCFormat;
                           Const _palette : TPTCPalette;
                           Const source, destination : TPTCArea);

Begin
  check;
  console.save(pixels, _width, _height, _pitch, _format, _palette,
               source, destination);
End;

Procedure TPTCConsole.clear;

Begin
  check;
  console.clear;
End;

Procedure TPTCConsole.clear(Const color : TPTCColor);

Begin
  check;
  console.clear(color);
End;

Procedure TPTCConsole.clear(Const color : TPTCColor;
                           Const _area : TPTCArea);

Begin
  check;
  console.clear(color, _area);
End;

Procedure TPTCConsole.palette(Const _palette : TPTCPalette);

Begin
  check;
  console.palette(_palette);
End;

Function TPTCConsole.palette : TPTCPalette;

Begin
  check;
  palette := console.palette;
End;

Procedure TPTCConsole.clip(Const _area : TPTCArea);

Begin
  check;
  console.clip(_area);
End;

Function TPTCConsole.width : Integer;

Begin
  check;
  width := console.width;
End;

Function TPTCConsole.height : Integer;

Begin
  check;
  height := console.height;
End;

Function TPTCConsole.pitch : Integer;

Begin
  check;
  pitch := console.pitch;
End;

Function TPTCConsole.pages : Integer;

Begin
  check;
  pages := console.pages;
End;

Function TPTCConsole.area : TPTCArea;

Begin
  check;
  area := console.area;
End;

Function TPTCConsole.clip : TPTCArea;

Begin
  check;
  clip := console.clip;
End;

Function TPTCConsole.format : TPTCFormat;

Begin
  check;
  format := console.format;
End;

Function TPTCConsole.name : String;

Begin
  name := '';
  If Assigned(console) Then
    name := console.name
  Else
{$IFDEF GO32V2}
    name := 'DOS';
{$ENDIF GO32V2}
{$IFDEF WIN32}
    name := 'Win32';
{$ENDIF WIN32}
{$IFDEF LINUX}
    name := 'Linux';
{$ENDIF LINUX}
End;

Function TPTCConsole.title : String;

Begin
  check;
  title := console.title;
End;

Function TPTCConsole.information : String;

Begin
  check;
  information := console.information;
End;

Function TPTCConsole.ConsoleCreate(index : Integer) : TPTCBaseConsole;

Begin
  {$IFDEF GO32V2}
  Case index Of
    0 : ConsoleCreate := VESAConsole.Create;
    1 : ConsoleCreate := VGAConsole.Create;
    2 : ConsoleCreate := CGAConsole.Create;
    3 : ConsoleCreate := TEXTFX2Console.Create;
    Else
      ConsoleCreate := Nil;
  End;
  {$ENDIF GO32V2}
  {$IFDEF WIN32}
  Case index Of
    0 : ConsoleCreate := TDirectXConsole.Create;
    Else
      ConsoleCreate := Nil;
  End;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  Case index Of
    0 : ConsoleCreate := TX11Console.Create;
    Else
      ConsoleCreate := Nil;
  End;
  {$ENDIF UNIX}
  If ConsoleCreate <> Nil Then
    ConsoleCreate.KeyReleaseEnabled := KeyReleaseEnabled;
End;

Function TPTCConsole.ConsoleCreate(Const _name : String) : TPTCBaseConsole;

Begin
  ConsoleCreate := Nil;
  {$IFDEF GO32V2}
  If _name = 'VESA' Then
    ConsoleCreate := VESAConsole.Create;
  If (_name = 'VGA') Or (_name = 'Fakemode') Then
    ConsoleCreate := VGAConsole.Create;
  If (_name = 'TEXTFX2') Or (_name = 'Text') Then
    ConsoleCreate := TEXTFX2Console.Create;
  If _name = 'CGA' Then
    ConsoleCreate := CGAConsole.Create;
  {$ENDIF GO32V2}
  {$IFDEF WIN32}
  If _name = 'DirectX' Then
    ConsoleCreate := TDirectXConsole.Create;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  If _name = 'X11' Then
    ConsoleCreate := TX11Console.Create;
  {$ENDIF UNIX}
  If ConsoleCreate <> Nil Then
    ConsoleCreate.KeyReleaseEnabled := KeyReleaseEnabled;
End;

Procedure TPTCConsole.check;

Begin
  {$IFDEF DEBUG}
  If console = Nil Then
    Raise TPTCError.Create('console is not open (core)');
  {$ENDIF DEBUG}
End;

{$WARNING this should be removed for fpc 1.1}
{pages=0}
Procedure TPTCConsole.open(Const _title : String);

Begin
  open(_title, 0);
End;

Procedure TPTCConsole.open(Const _title : String; Const _format : TPTCFormat);

Begin
  open(_title, _format, 0);
End;

Procedure TPTCConsole.open(Const _title : String; _width, _height : Integer;
                           Const _format : TPTCFormat);

Begin
  open(_title, _width, _height, _format, 0);
End;

Procedure TPTCConsole.open(Const _title : String; Const _mode : TPTCMode);

Begin
  open(_title, _mode, 0);
End;
{/pages=0}
