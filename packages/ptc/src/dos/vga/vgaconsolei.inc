{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$MACRO ON}

{$DEFINE DEFAULT_WIDTH:=320}
{$DEFINE DEFAULT_HEIGHT:=200}
{$DEFINE DEFAULT_FORMAT:=TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF)}

{$ASMMODE intel}

constructor TVGAConsole.Create;

var
  fmt1, fmt2, fmt3: TPTCFormat;

begin
  inherited Create;

  m_open := False;
  m_locked := False;
  m_title := '';
  m_information := '';
  m_default_width := DEFAULT_WIDTH;
  m_default_height := DEFAULT_HEIGHT;
  m_default_format := DEFAULT_FORMAT;

  m_primary := nil;

  m_area := TPTCArea.Create;
  m_clip := TPTCArea.Create;
  m_copy := TPTCCopy.Create;
  m_clear := TPTCClear.Create;
  m_palette := TPTCPalette.Create;

  fmt1 := nil;
  fmt2 := nil;
  fmt3 := nil;
  try
    fmt1 := TPTCFormat.Create(8);
    fmt2 := TPTCFormat.Create(8, $E0, $1C, $03);
    fmt3 := TPTCFormat.Create(16, $F800, $7E0, $1F);
    m_modes[0] := TPTCMode.Create(320, 200, fmt1);
    m_modes[1] := TPTCMode.Create(320, 200, fmt2);
    m_modes[2] := TPTCMode.Create(320, 200, fmt3);
    m_modes[3] := TPTCMode.Create;
  finally
    fmt1.Free;
    fmt2.Free;
    fmt3.Free;
  end;
  m_faketype := FAKEMODE2A;

  configure('ptcpas.cfg');
end;

destructor TVGAConsole.Destroy;

begin
  close;
  internal_clear_mode_list;
  m_keyboard.Free;
  FMouse.Free;
  FEventQueue.Free;
  m_copy.Free;
  m_clear.Free;
  m_default_format.Free;
  m_palette.Free;
  m_clip.Free;
  m_area.Free;
  inherited Destroy;
end;

procedure TVGAConsole.Configure(const AFileName: String);

var
  F: Text;
  S: string;

begin
  AssignFile(F, AFileName);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$I-}
    Readln(F, S);
    {$I+}
    if IOResult <> 0 then
      Break;
    Option(S);
  end;
  CloseFile(F);
end;

function TVGAConsole.option(const _option: String): Boolean;

begin
  {...}
  if (System.Copy(_option, 1, 8) = 'FAKEMODE') and (Length(_option) = 10) and
    (_option[9] >= '1') and (_option[9] <= '3') and
    (_option[10] >= 'A') and (_option[10] <= 'C') then
  begin
    case _option[9] of
      '1': case _option[10] of
        'A': m_faketype := FAKEMODE1A;
        'B': m_faketype := FAKEMODE1B;
        'C': m_faketype := FAKEMODE1C;
      end;
      '2': case _option[10] of
        'A': m_faketype := FAKEMODE2A;
        'B': m_faketype := FAKEMODE2B;
        'C': m_faketype := FAKEMODE2C;
      end;
      '3': case _option[10] of
        'A': m_faketype := FAKEMODE3A;
        'B': m_faketype := FAKEMODE3B;
        'C': m_faketype := FAKEMODE3C;
      end;
    end;
    Result := True;
    exit;
  end;
  if _option = 'enable logging' then
  begin
    LOG_enabled := True;
    Result := True;
    exit;
  end;
  if _option = 'disable logging' then
  begin
    LOG_enabled := False;
    Result := True;
    exit;
  end;

  Result := m_copy.option(_option);
end;

procedure TVGAConsole.internal_clear_mode_list;

var
  I: Integer;
  Done: Boolean;

begin
  I := 0;
  Done := False;
  repeat
    Done := not m_modes[I].valid;
    m_modes[I].Free;
    Inc(I);
  until Done;
end;

function TVGAConsole.modes: PPTCMode;

begin
  Result := m_modes;
end;

procedure TVGAConsole.open(const _title: string; _pages: Integer); overload;

begin
  open(_title, m_default_format, _pages);
end;

procedure TVGAConsole.open(const _title: string; const _format: TPTCFormat;
                           _pages: Integer); overload;

begin
  open(_title, m_default_width, m_default_height, _format, _pages);
end;

procedure TVGAConsole.open(const _title: string; _width, _height: Integer;
                           const _format: TPTCFormat; _pages: Integer); overload;

var
  m: TPTCMode;

begin
  m := TPTCMode.Create(_width, _height, _format);
  try
    open(_title, m, _pages);
  finally
    m.Free;
  end;
end;

procedure TVGAConsole.open(const _title: string; const _mode: TPTCMode;
                           _pages: Integer); overload;

var
  I: Integer;
  modetype: Integer;

begin
  if not _mode.valid then
    raise TPTCError.Create('invalid mode');
  if _mode.format.indexed then
    modetype := INDEX8
  else
    if _mode.format.bits = 8 then
      modetype := RGB332
    else
      modetype := FAKEMODE;
  internal_pre_open_setup(_title);
  internal_open_fullscreen_start;
  internal_open_fullscreen(modetype);
  internal_open_fullscreen_finish(_pages);
  internal_post_open_setup;
end;

procedure TVGAConsole.close;

begin
  if m_open then
  begin
    if m_locked then
      raise TPTCError.Create('console is still locked');
    { flush all key presses }
    while KeyPressed do ReadKey;
    internal_close;
    m_open := False;
  end;
end;

procedure TVGAConsole.flush;

begin
  check_open;
  check_unlocked;
end;

procedure TVGAConsole.finish;

begin
  check_open;
  check_unlocked;
end;

procedure TVGAConsole.vga_load(data: Pointer); ASSembler; Register;

Asm
  push es
  mov esi, data
  mov ax, fs
  mov es, ax
  mov ecx, 64000/4
  mov edi, 0A0000h
  cld
  rep movsd
  pop es
end;

procedure TVGAConsole.update;

begin
  check_open;
  check_unlocked;
  case m_CurrentMode of
    0, 1: begin
      while (inportb($3DA) and 8) <> 0 Do;
      while (inportb($3DA) and 8) = 0 Do;
      vga_load(m_primary);
    end;
    2: fakemode_load(m_primary, True);
  end;
end;

procedure TVGAConsole.update(const _area: TPTCArea);

begin
  update;
end;

procedure TVGAConsole.copy(surface: TPTCBaseSurface);

var
  pixels: Pointer;

begin
  check_open;
  check_unlocked;
  pixels := lock;
  try
    try
      surface.load(pixels, width, height, pitch, format, palette);
    finally
      unlock;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to copy console to surface', error);

  end;
end;

procedure TVGAConsole.copy(surface: TPTCBaseSurface;
                           const source, destination: TPTCArea);

var
  pixels: Pointer;

begin
  check_open;
  check_unlocked;
  pixels := lock;
  try
    try
      surface.load(pixels, width, height, pitch, format, palette, source, destination);
    finally
      unlock;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to copy console to surface', error);

  end;
end;

function TVGAConsole.lock: Pointer;

begin
  check_open;
  if m_locked then
    raise TPTCError.Create('console is already locked');

  m_locked := True;
  Result := m_primary;
end;

procedure TVGAConsole.unlock;

begin
  check_open;
  if not m_locked then
    raise TPTCError.Create('console is not locked');

  m_locked := False;
end;

procedure TVGAConsole.load(const pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           const _format: TPTCFormat;
                           const _palette: TPTCPalette);
var
  Area_: TPTCArea;
  console_pixels: Pointer;

begin
  check_open;
  check_unlocked;
  if clip.Equals(area) then
  begin
    try
      console_pixels := lock;
      try
        m_copy.request(_format, format);
        m_copy.palette(_palette, palette);
        m_copy.copy(pixels, 0, 0, _width, _height, _pitch, console_pixels, 0, 0,
                    width, height, pitch);
      finally
        unlock;
      end;
    except
      on error: TPTCError do
        raise TPTCError.Create('failed to load pixels to console', error);

    end;
  end
  else
  begin
    Area_ := TPTCArea.Create(0, 0, width, height);
    try
      load(pixels, _width, _height, _pitch, _format, _palette, Area_, area);
    finally
      Area_.Free;
    end;
  end;
end;

procedure TVGAConsole.load(const pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           const _format: TPTCFormat;
                           const _palette: TPTCPalette;
                           const source, destination: TPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: TPTCArea;
  tmp: TPTCArea;

begin
  check_open;
  check_unlocked;
  clipped_source := nil;
  clipped_destination := nil;
  try
    console_pixels := lock;
    try
      clipped_source := TPTCArea.Create;
      clipped_destination := TPTCArea.Create;
      tmp := TPTCArea.Create(0, 0, _width, _height);
      try
        TPTCClipper.clip(source, tmp, clipped_source, destination, clip, clipped_destination);
      finally
        tmp.Free;
      end;
      m_copy.request(_format, format);
      m_copy.palette(_palette, palette);
      m_copy.copy(pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, _pitch,
                  console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, pitch);
    finally
      unlock;
      clipped_source.Free;
      clipped_destination.Free;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to load pixels to console area', error);

  end;
end;

procedure TVGAConsole.save(pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           const _format: TPTCFormat;
                           const _palette: TPTCPalette);
var
  Area_: TPTCArea;
  console_pixels: Pointer;

begin
  check_open;
  check_unlocked;
  if clip.Equals(area) then
  begin
    try
      console_pixels := lock;
      try
        m_copy.request(format, _format);
        m_copy.palette(palette, _palette);
        m_copy.copy(console_pixels, 0, 0, width, height, pitch, pixels, 0, 0,
                    _width, _height, _pitch);
      finally
        unlock;
      end;
    except
      on error: TPTCError do
        raise TPTCError.Create('failed to save console pixels', error);

    end;
  end
  else
  begin
    Area_ := TPTCArea.Create(0, 0, width, height);
    try
      save(pixels, _width, _height, _pitch, _format, _palette, area, Area_);
    finally
      Area_.Free;
    end;
  end;
end;

procedure TVGAConsole.save(pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           const _format: TPTCFormat;
                           const _palette: TPTCPalette;
                           const source, destination: TPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: TPTCArea;
  tmp: TPTCArea;

begin
  check_open;
  check_unlocked;
  clipped_source := nil;
  clipped_destination := nil;
  try
    console_pixels := lock;
    try
      clipped_source := TPTCArea.Create;
      clipped_destination := TPTCArea.Create;
      tmp := TPTCArea.Create(0, 0, _width, _height);
      try
        TPTCClipper.clip(source, clip, clipped_source, destination, tmp, clipped_destination);
      finally
        tmp.Free;
      end;
      m_copy.request(format, _format);
      m_copy.palette(palette, _palette);
      m_copy.copy(console_pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, pitch,
                  pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, _pitch);
    finally
      unlock;
      clipped_source.Free;
      clipped_destination.Free;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to save console area pixels', error);

  end;
end;

procedure TVGAConsole.clear;

var
  tmp: TPTCColor;

begin
  check_open;
  check_unlocked;
  if format.direct then
    tmp := TPTCColor.Create(0, 0, 0, 0)
  else
    tmp := TPTCColor.Create(0);
  try
    clear(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TVGAConsole.clear(const color: TPTCColor);

var
  tmp: TPTCArea;

begin
  check_open;
  check_unlocked;
  tmp := TPTCArea.Create;
  try
    clear(color, tmp);
  finally
    tmp.Free;
  end;
end;

procedure TVGAConsole.clear(const color: TPTCColor;
                            const _area: TPTCArea);

var
  pixels: Pointer;
  clipped_area: TPTCArea;

begin
  check_open;
  check_unlocked;
  try
    clipped_area := nil;
    pixels := lock;
    try
      clipped_area := TPTCClipper.clip(_area, clip);
      m_clear.request(format);
      m_clear.clear(pixels, clipped_area.left, clipped_area.right, clipped_area.width, clipped_area.height, pitch, color);
    finally
      unlock;
      clipped_area.Free;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to clear console area', error);

  end;
end;

procedure TVGAConsole.Palette(const _palette: TPTCPalette);

begin
  check_open;
  if format.indexed then
  begin
    m_palette.load(_palette.data);
    internal_SetPalette(_palette.data);
  end;
end;

function TVGAConsole.Palette: TPTCPalette;

begin
  check_open;
  Result := m_palette;
end;

procedure TVGAConsole.Clip(const _area: TPTCArea);

var
  tmp: TPTCArea;

begin
  check_open;
  tmp := TPTCClipper.clip(_area, m_area);
  try
    m_clip.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

function TVGAConsole.GetWidth: Integer;

begin
  check_open;
  Result := m_width;
end;

function TVGAConsole.GetHeight: Integer;

begin
  check_open;
  Result := m_height;
end;

function TVGAConsole.GetPitch: Integer;

begin
  check_open;
  Result := m_pitch;
end;

function TVGAConsole.GetPages: Integer;

begin
  check_open;
  Result := 2;
end;

function TVGAConsole.GetArea: TPTCArea;

begin
  check_open;
  Result := m_area;
end;

function TVGAConsole.Clip: TPTCArea;

begin
  check_open;
  Result := m_clip;
end;

function TVGAConsole.GetFormat: TPTCFormat;

begin
  check_open;
  Result := m_modes[m_CurrentMode].format;
end;

function TVGAConsole.GetName: string;

begin
  Result := 'VGA';
end;

function TVGAConsole.GetTitle: string;

begin
  Result := m_title;
end;

function TVGAConsole.GetInformation: string;

begin
  Result := m_information;
end;

procedure TVGAConsole.internal_pre_open_setup(const _title: String);

begin
  m_title := _title;
end;

procedure TVGAConsole.internal_open_fullscreen_start;

begin
end;

procedure TVGAConsole.internal_open_fullscreen(ModeType: Integer);

var
  tmp: TPTCArea;

begin
  VGASetMode(320, 200, ModeType, m_faketype);
  case ModeType of
    INDEX8: begin
      m_CurrentMode := 0;
      m_pitch := 320;
    end;
    RGB332: begin
      m_CurrentMode := 1;
      m_pitch := 320;
    end;
    FAKEMODE: begin
      m_CurrentMode := 2;
      m_pitch := 640;
    end;
  end;
  m_width := 320;
  m_height := 200;

  tmp := TPTCArea.Create(0, 0, m_width, m_height);
  try
    m_area.Assign(tmp);
    m_clip.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TVGAConsole.internal_open_fullscreen_finish(_pages: Integer);

begin
  FreeMemAndNil(m_primary);
  m_primary := GetMem(m_height * m_pitch);
  FillChar(m_primary^, m_height * m_pitch, 0);
end;

procedure TVGAConsole.internal_post_open_setup;

begin
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  m_keyboard := TDosKeyboard.Create;
  FMouse := TDosMouse.Create(m_width, m_height);
  FEventQueue := TEventQueue.Create;

  { temporary platform dependent information fudge }
  m_information := 'dos version x.xx.x, VGA, 320x200 FAKEMODExx/8-bit indexed/8-bit RGB332';

  { set open flag }
  m_open := True;
end;

procedure TVGAConsole.internal_reset;

begin
  FreeMemAndNil(m_primary);
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
end;

procedure TVGAConsole.internal_close;

begin
  FreeMemAndNil(m_primary);
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  RestoreTextMode;
end;

procedure TVGAConsole.internal_SetPalette(data: PUint32);

var
  i: Integer;
  c: DWord;

begin
  outportb($3C8, 0);
  for i := 0 to 255 do
  begin
    c := (data^ shr 2) and $003F3F3F;
    outportb($3C9, Byte(c shr 16));
    outportb($3C9, Byte(c shr 8));
    outportb($3C9, Byte(c));
    Inc(data);
  end;
end;

procedure TVGAConsole.HandleEvents;

begin
  m_keyboard.GetPendingEvents(FEventQueue);
  FMouse.GetPendingEvents(FEventQueue);
end;

function TVGAConsole.NextEvent(var event: TPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean;

begin
  check_open;

  FreeAndNil(event);
  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    event := FEventQueue.NextEvent(EventMask);
  until (not Wait) or (event <> Nil);
  Result := event <> nil;
end;

function TVGAConsole.PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): TPTCEvent;

begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(EventMask);
  until (not Wait) or (Result <> Nil);
end;

procedure TVGAConsole.check_open;

begin
  if not m_open then
    raise TPTCError.Create('console is not open');
end;

procedure TVGAConsole.check_unlocked;

begin
  if m_locked then
    raise TPTCError.Create('console is not unlocked');
end;
