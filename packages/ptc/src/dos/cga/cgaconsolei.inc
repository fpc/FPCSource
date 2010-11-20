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

constructor TCGAConsole.Create;

var
  I: Integer;

begin
  inherited Create;

  m_open := False;
  m_locked := False;
  FillChar(m_modes, SizeOf(m_modes), 0);
  m_title := '';
  m_information := '';
  m_default_width := DEFAULT_WIDTH;
  m_default_height := DEFAULT_HEIGHT;
  m_default_format := DEFAULT_FORMAT;

  for I := 0 to 255 do
    m_modes[I] := TPTCMode.Create;

  m_copy := TPTCCopy.Create;
  m_clear := TPTCClear.Create;
  Configure('ptcpas.cfg');
end;

destructor TCGAConsole.Destroy;

var
  I: Integer;

begin
  close;
  for I := 0 to 255 do
    m_modes[I].Free;
  m_keyboard.Free;
  FMouse.Free;
  FEventQueue.Free;
  m_copy.Free;
  m_clear.Free;
  m_default_format.Free;
  inherited Destroy;
end;

procedure TCGAConsole.Configure(const AFileName: String);

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

function TCGAConsole.option(const _option: String): Boolean;

begin
  {...}
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

function TCGAConsole.modes: PPTCMode;

begin
  Result := @m_modes;
end;

procedure TCGAConsole.open(const _title: string; _pages: Integer); overload;

begin
  open(_title, m_default_format, _pages);
end;

procedure TCGAConsole.open(const _title: string; const _format: TPTCFormat;
                           _pages: Integer); overload;

begin
  open(_title, m_default_width, m_default_height, _format, _pages);
end;

procedure TCGAConsole.open(const _title: string; _width, _height: Integer;
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

procedure TCGAConsole.open(const _title: string; const _mode: TPTCMode;
                           _pages: Integer); overload;

var
  _width, _height: Integer;
  _format: TPTCFormat;

begin
  if not _mode.valid then
    raise TPTCError.Create('invalid mode');

  _width := _mode.width;
  _height := _mode.height;
  _format := _mode.format;

  internal_pre_open_setup(_title);
  internal_open_fullscreen_start;
  internal_open_fullscreen(_width, _height, _format);
  internal_open_fullscreen_finish(_pages);
  internal_post_open_setup;
end;

procedure TCGAConsole.close;

begin
  if m_open then
  begin
    if m_locked then
      raise TPTCError.Create('console is still locked');
    {flush all key presses}
    while KeyPressed do ReadKey;
    internal_close;
    m_open := False;
  end;
end;

procedure TCGAConsole.flush;

begin
  check_open;
  check_unlocked;
end;

procedure TCGAConsole.finish;

begin
  check_open;
  check_unlocked;
end;

procedure TCGAConsole.update;

var
  framebuffer: PByte;

begin
  check_open;
  check_unlocked;
  framebuffer := m_primary.lock;
  try
{    vrc;}
    CGADump(framebuffer);
  finally
    m_primary.unlock;
  end;
end;

procedure TCGAConsole.update(const _area: TPTCArea);

begin
  update;
end;

procedure TCGAConsole.copy(surface: TPTCBaseSurface);

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

procedure TCGAConsole.copy(surface: TPTCBaseSurface;
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

function TCGAConsole.lock: Pointer;

var
  pixels: Pointer;

begin
  check_open;
  if m_locked then
    raise TPTCError.Create('console is already locked');

  pixels := m_primary.lock;
  m_locked := True;
  Result := pixels;
end;

procedure TCGAConsole.unlock;

begin
  check_open;
  if not m_locked then
    raise TPTCError.Create('console is not locked');

  m_primary.unlock;
  m_locked := False;
end;

procedure TCGAConsole.load(const pixels: Pointer;
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

procedure TCGAConsole.load(const pixels: Pointer;
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

procedure TCGAConsole.save(pixels: Pointer;
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

procedure TCGAConsole.save(pixels: Pointer;
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

procedure TCGAConsole.clear;

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

procedure TCGAConsole.clear(const color: TPTCColor);

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

procedure TCGAConsole.clear(const color: TPTCColor;
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

procedure TCGAConsole.Palette(const _palette: TPTCPalette);

begin
  check_open;
  m_primary.palette(_palette);
end;

function TCGAConsole.Palette: TPTCPalette;

begin
  check_open;
  Result := m_primary.palette;
end;

procedure TCGAConsole.Clip(const _area: TPTCArea);

begin
  check_open;
  m_primary.clip(_area);
end;

function TCGAConsole.GetWidth: Integer;

begin
  check_open;
  Result := m_primary.width;
end;

function TCGAConsole.GetHeight: Integer;

begin
  check_open;
  Result := m_primary.height;
end;

function TCGAConsole.GetPitch: Integer;

begin
  check_open;
  Result := m_primary.pitch;
end;

function TCGAConsole.GetPages: Integer;

begin
  check_open;
  Result := 2;
end;

function TCGAConsole.GetArea: TPTCArea;

begin
  check_open;
  Result := m_primary.area;
end;

function TCGAConsole.Clip: TPTCArea;

begin
  check_open;
  Result := m_primary.clip;
end;

function TCGAConsole.GetFormat: TPTCFormat;

begin
  check_open;
  Result := m_primary.format;
end;

function TCGAConsole.GetName: string;

begin
  Result := 'CGA';
end;

function TCGAConsole.GetTitle: string;

begin
  Result := m_title;
end;

function TCGAConsole.GetInformation: string;

begin
  Result := m_information;
end;

procedure TCGAConsole.internal_pre_open_setup(const _title: String);

begin
  m_title := _title;
end;

procedure TCGAConsole.internal_open_fullscreen_start;

var
  f: TPTCFormat;

begin
  CGAPrecalc;

  f := TPTCFormat.Create(32, $FF0000, $00FF00, $0000FF);
  try
    m_primary := TPTCSurface.Create(320, 200, f);
  finally
    f.Free;
  end;

  CGA320;
end;

procedure TCGAConsole.internal_open_fullscreen(_width, _height: Integer; const _format: TPTCFormat);

begin
end;

procedure TCGAConsole.internal_open_fullscreen_finish(_pages: Integer);

begin
end;

procedure TCGAConsole.internal_post_open_setup;

begin
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  m_keyboard := TDosKeyboard.Create;
  FMouse := TDosMouse.Create(m_primary.width, m_primary.height);
  FEventQueue := TEventQueue.Create;

  { temporary platform dependent information fudge }
  m_information := 'dos version x.xx.x, CGA, 320x200 - 4 colors';

  { set open flag }
  m_open := True;
end;

procedure TCGAConsole.internal_reset;

begin
  FreeAndNil(m_primary);
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
end;

procedure TCGAConsole.internal_close;

begin
  FreeAndNil(m_primary);
  FreeAndNil(m_keyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);

  CGAText;
end;

procedure TCGAConsole.HandleEvents;

begin
  m_keyboard.GetPendingEvents(FEventQueue);
  FMouse.GetPendingEvents(FEventQueue);
end;

function TCGAConsole.NextEvent(var event: TPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean;

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

function TCGAConsole.PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): TPTCEvent;

begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(EventMask);
  until (not Wait) or (Result <> Nil);
end;

procedure TCGAConsole.check_open;

begin
  if not m_open then
    raise TPTCError.Create('console is not open');
end;

procedure TCGAConsole.check_unlocked;

begin
  if m_locked then
    raise TPTCError.Create('console is not unlocked');
end;
