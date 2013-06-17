{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2011 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

  SetLength(m_modes, 3);
  m_modes[0] := TPTCMode.Create(320, 200, TPTCFormat.Create(8));
  m_modes[1] := TPTCMode.Create(320, 200, TPTCFormat.Create(8, $E0, $1C, $03));
  m_modes[2] := TPTCMode.Create(320, 200, TPTCFormat.Create(16, $F800, $7E0, $1F));

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
  inherited Destroy;
end;

procedure TVGAConsole.Configure(const AFileName: string);
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, AFileName);
  {$push}{$I-}
  Reset(F);
  {$pop}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$push}{$I-}
    Readln(F, S);
    {$pop}
    if IOResult <> 0 then
      Break;
    Option(S);
  end;
  CloseFile(F);
end;

function TVGAConsole.option(const _option: string): Boolean;
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
begin
  SetLength(m_modes, 0);
end;

function TVGAConsole.modes: TPTCModeList;
begin
  Result := m_modes;
end;

procedure TVGAConsole.open(const _title: string; _pages: Integer); overload;
begin
  open(_title, m_default_format, _pages);
end;

procedure TVGAConsole.open(const _title: string; _format: IPTCFormat;
                           _pages: Integer); overload;
begin
  open(_title, m_default_width, m_default_height, _format, _pages);
end;

procedure TVGAConsole.open(const _title: string; _width, _height: Integer;
                           _format: IPTCFormat; _pages: Integer); overload;
begin
  open(_title, TPTCMode.Create(_width, _height, _format), _pages);
end;

procedure TVGAConsole.open(const _title: string; _mode: IPTCMode;
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

procedure TVGAConsole.vga_load(data: Pointer); ASSembler; register;
asm
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

procedure TVGAConsole.update(_area: IPTCArea);
begin
  update;
end;

procedure TVGAConsole.copy(surface: IPTCSurface);
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

procedure TVGAConsole.copy(surface: IPTCSurface;
                           source, destination: IPTCArea);
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

procedure TVGAConsole.Load(const pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           _format: IPTCFormat;
                           _palette: IPTCPalette);
var
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
    Load(pixels, _width, _height, _pitch, _format, _palette, TPTCArea.Create(0, 0, width, height), area);
end;

procedure TVGAConsole.Load(const pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           _format: IPTCFormat;
                           _palette: IPTCPalette;
                           source, destination: IPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: IPTCArea;
begin
  check_open;
  check_unlocked;
  try
    console_pixels := lock;
    try
      TPTCClipper.clip(source, TPTCArea.Create(0, 0, _width, _height), clipped_source, destination, clip, clipped_destination);
      m_copy.request(_format, format);
      m_copy.palette(_palette, palette);
      m_copy.copy(pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, _pitch,
                  console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, pitch);
    finally
      unlock;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to load pixels to console area', error);
  end;
end;

procedure TVGAConsole.save(pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           _format: IPTCFormat;
                           _palette: IPTCPalette);
var
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
    Save(pixels, _width, _height, _pitch, _format, _palette, area, TPTCArea.Create(0, 0, width, height));
end;

procedure TVGAConsole.Save(pixels: Pointer;
                           _width, _height, _pitch: Integer;
                           _format: IPTCFormat;
                           _palette: IPTCPalette;
                           source, destination: IPTCArea);
var
  console_pixels: Pointer;
  clipped_source, clipped_destination: IPTCArea;
begin
  check_open;
  check_unlocked;
  try
    console_pixels := lock;
    try
      TPTCClipper.clip(source, clip, clipped_source, destination, TPTCArea.Create(0, 0, _width, _height), clipped_destination);
      m_copy.request(format, _format);
      m_copy.palette(palette, _palette);
      m_copy.copy(console_pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, pitch,
                  pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, _pitch);
    finally
      unlock;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to save console area pixels', error);
  end;
end;

procedure TVGAConsole.clear;
var
  Color: IPTCColor;
begin
  check_open;
  check_unlocked;
  if format.direct then
    Color := TPTCColor.Create(0, 0, 0, 0)
  else
    Color := TPTCColor.Create(0);
  Clear(Color);
end;

procedure TVGAConsole.clear(AColor: IPTCColor);
begin
  check_open;
  check_unlocked;
  Clear(AColor, TPTCArea.Create);
end;

procedure TVGAConsole.clear(color: IPTCColor;
                            _area: IPTCArea);
var
  pixels: Pointer;
  clipped_area: IPTCArea;
begin
  check_open;
  check_unlocked;
  try
    pixels := lock;
    try
      clipped_area := TPTCClipper.clip(_area, clip);
      m_clear.request(format);
      m_clear.clear(pixels, clipped_area.left, clipped_area.right, clipped_area.width, clipped_area.height, pitch, color);
    finally
      unlock;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to clear console area', error);
  end;
end;

procedure TVGAConsole.Palette(_palette: IPTCPalette);
begin
  check_open;
  if format.indexed then
  begin
    m_palette.load(_palette.data);
    internal_SetPalette(_palette.data);
  end;
end;

function TVGAConsole.Palette: IPTCPalette;
begin
  check_open;
  Result := m_palette;
end;

procedure TVGAConsole.Clip(_area: IPTCArea);
begin
  check_open;
  m_clip := TPTCClipper.clip(_area, m_area);
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

function TVGAConsole.GetArea: IPTCArea;
begin
  check_open;
  Result := m_area;
end;

function TVGAConsole.Clip: IPTCArea;
begin
  check_open;
  Result := m_clip;
end;

function TVGAConsole.GetFormat: IPTCFormat;
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

procedure TVGAConsole.internal_pre_open_setup(const _title: string);
begin
  m_title := _title;
end;

procedure TVGAConsole.internal_open_fullscreen_start;
begin
end;

procedure TVGAConsole.internal_open_fullscreen(ModeType: Integer);
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

  m_area := TPTCArea.Create(0, 0, m_width, m_height);
  m_clip := m_area;
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

function TVGAConsole.NextEvent(out event: IPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean;
begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    event := FEventQueue.NextEvent(EventMask);
  until (not Wait) or (event <> Nil);
  Result := event <> nil;
end;

function TVGAConsole.PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): IPTCEvent;
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
