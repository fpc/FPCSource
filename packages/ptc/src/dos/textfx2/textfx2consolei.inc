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

constructor TTextFX2Console.Create;
begin
  inherited Create;

  FOpen := False;
  FLocked := False;
  FTitle := '';
  FInformation := '';
  FDefaultWidth := DEFAULT_WIDTH;
  FDefaultHeight := DEFAULT_HEIGHT;
  FDefaultFormat := DEFAULT_FORMAT;

  calcpal := @calcpal_colorbase;
  use_charset := @charset_b7asc;
  build_colormap(0);
  FCopy := TPTCCopy.Create;
  FClear := TPTCClear.Create;
  Configure('ptcpas.cfg');
end;

destructor TTextFX2Console.Destroy;
begin
  Close;
  F160x100buffer.Free;
  FPrimary.Free;

  FKeyboard.Free;
  FMouse.Free;
  FEventQueue.Free;
  FCopy.Free;
  FClear.Free;
  dispose_colormap;
  inherited Destroy;
end;

procedure TTextFX2Console.Configure(const AFileName: string);
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

function TTextFX2Console.Option(const AOption: string): Boolean;
begin
  {...}
  Result := True;
  if AOption = 'charset_b8ibm' then
  begin
    use_charset := @charset_b8ibm;
    exit;
  end;
  if AOption = 'charset_b7asc' then
  begin
    use_charset := @charset_b7asc;
    exit;
  end;
  if AOption = 'charset_b7sml' then
  begin
    use_charset := @charset_b7sml;
    exit;
  end;
  if AOption = 'charset_b8gry' then
  begin
    use_charset := @charset_b8gry;
    exit;
  end;
  if AOption = 'charset_b7nws' then
  begin
    use_charset := @charset_b7nws;
    exit;
  end;
  if AOption = 'calcpal_colorbase' then
  begin
    calcpal := @calcpal_colorbase;
    build_colormap(0);
    exit;
  end;
  if AOption = 'calcpal_lightbase' then
  begin
    calcpal := @calcpal_lightbase;
    build_colormap(0);
    exit;
  end;
  if AOption = 'calcpal_lightbase_g' then
  begin
    calcpal := @calcpal_lightbase_g;
    build_colormap(0);
    exit;
  end;
  if AOption = 'enable logging' then
  begin
    LOG_enabled := True;
    Result := True;
    exit;
  end;
  if AOption = 'disable logging' then
  begin
    LOG_enabled := False;
    Result := True;
    exit;
  end;

  Result := FCopy.Option(AOption);
end;

function TTextFX2Console.Modes: TPTCModeList;
begin
  Result := FModes;
end;

procedure TTextFX2Console.Open(const _title: string; _pages: Integer); overload;
begin
  Open(_title, FDefaultFormat, _pages);
end;

procedure TTextFX2Console.open(const _title: string; _format: IPTCFormat;
                               _pages: Integer); overload;
begin
  open(_title, FDefaultWidth, FDefaultHeight, _format, _pages);
end;

procedure TTextFX2Console.open(const _title: string; _width, _height: Integer;
                               _format: IPTCFormat; _pages: Integer); overload;
begin
  open(_title, TPTCMode.Create(_width, _height, _format), _pages);
end;

procedure TTextFX2Console.open(const _title: string; _mode: IPTCMode;
                               _pages: Integer); overload;
var
  _width, _height: Integer;
  _format: IPTCFormat;
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

procedure TTextFX2Console.Close;
begin
  if FOpen then
  begin
    if FLocked then
      raise TPTCError.Create('console is still locked');
    {flush all key presses}
    while KeyPressed do ReadKey;
    internal_close;
    FOpen := False;
  end;
end;

procedure TTextFX2Console.flush;
begin
  check_open;
  check_unlocked;
end;

procedure TTextFX2Console.finish;
begin
  check_open;
  check_unlocked;
end;

procedure TTextFX2Console.update;
var
  framebuffer: PInteger;
begin
  check_open;
  check_unlocked;

  FPrimary.copy(F160x100buffer);
  framebuffer := F160x100buffer.Lock;
  vrc;
  dump_160x(0, 50, framebuffer);
  F160x100buffer.Unlock;
end;

procedure TTextFX2Console.update(_area: IPTCArea);
begin
  update;
end;

procedure TTextFX2Console.copy(surface: IPTCSurface);
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

procedure TTextFX2Console.copy(surface: IPTCSurface;
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

function TTextFX2Console.Lock: Pointer;
var
  pixels: Pointer;
begin
  check_open;
  if FLocked then
    raise TPTCError.Create('console is already locked');
  pixels := FPrimary.lock;
  FLocked := True;
  Result := pixels;
end;

procedure TTextFX2Console.Unlock;
begin
  check_open;
  if not FLocked then
    raise TPTCError.Create('console is not locked');

  FPrimary.unlock;
  FLocked := False;
end;

procedure TTextFX2Console.Load(const pixels: Pointer;
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
        FCopy.request(_format, format);
        FCopy.palette(_palette, palette);
        FCopy.copy(pixels, 0, 0, _width, _height, _pitch, console_pixels, 0, 0,
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

procedure TTextFX2Console.load(const pixels: Pointer;
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
      FCopy.request(_format, format);
      FCopy.palette(_palette, palette);
      FCopy.copy(pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, _pitch,
                  console_pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, pitch);
    finally
      unlock;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to load pixels to console area', error);

  end;
end;

procedure TTextFX2Console.save(pixels: Pointer;
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
        FCopy.request(format, _format);
        FCopy.palette(palette, _palette);
        FCopy.copy(console_pixels, 0, 0, width, height, pitch, pixels, 0, 0,
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

procedure TTextFX2Console.save(pixels: Pointer;
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
      FCopy.request(format, _format);
      FCopy.palette(palette, _palette);
      FCopy.copy(console_pixels, clipped_source.left, clipped_source.top, clipped_source.width, clipped_source.height, pitch,
                 pixels, clipped_destination.left, clipped_destination.top, clipped_destination.width, clipped_destination.height, _pitch);
    finally
      unlock;
    end;
  except
    on error:TPTCError do
      raise TPTCError.Create('failed to save console area pixels', error);
  end;
end;

procedure TTextFX2Console.clear;
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

procedure TTextFX2Console.Clear(AColor: IPTCColor);
begin
  check_open;
  check_unlocked;
  Clear(AColor, TPTCArea.Create);
end;

procedure TTextFX2Console.clear(color: IPTCColor;
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
      FClear.request(format);
      FClear.clear(pixels, clipped_area.left, clipped_area.right, clipped_area.width, clipped_area.height, pitch, color);
    finally
      unlock;
    end;
  except
    on error: TPTCError do
      raise TPTCError.Create('failed to clear console area', error);

  end;
end;

procedure TTextFX2Console.Palette(_palette: IPTCPalette);
begin
  check_open;
  FPrimary.palette(_palette);
end;

function TTextFX2Console.Palette: IPTCPalette;
begin
  check_open;
  Result := FPrimary.palette;
end;

procedure TTextFX2Console.Clip(_area: IPTCArea);
begin
  check_open;
  FPrimary.clip(_area);
end;

function TTextFX2Console.GetWidth: Integer;
begin
  check_open;
  Result := FPrimary.width;
end;

function TTextFX2Console.GetHeight: Integer;
begin
  check_open;
  Result := FPrimary.height;
end;

function TTextFX2Console.GetPitch: Integer;
begin
  check_open;
  Result := FPrimary.pitch;
end;

function TTextFX2Console.GetPages: Integer;
begin
  check_open;
  Result := 2;{FPrimary.pages;}
end;

function TTextFX2Console.GetArea: IPTCArea;
begin
  check_open;
  Result := FPrimary.area;
end;

function TTextFX2Console.Clip: IPTCArea;
begin
  check_open;
  Result := FPrimary.clip;
end;

function TTextFX2Console.GetFormat: IPTCFormat;
begin
  check_open;
  Result := FPrimary.format;
end;

function TTextFX2Console.GetName: string;
begin
  Result := 'TextFX2';
end;

function TTextFX2Console.GetTitle: string;
begin
  Result := FTitle;
end;

function TTextFX2Console.GetInformation: string;
begin
  Result := FInformation;
end;

procedure TTextFX2Console.internal_pre_open_setup(const _title: string);
begin
  FTitle := _title;
end;

procedure TTextFX2Console.internal_open_fullscreen_start;
begin
  F160x100buffer := TPTCSurface.Create(160, 100, TPTCFormat.Create(32, $0000FF, $00FF00, $FF0000));
  set80x50;
end;

procedure TTextFX2Console.internal_open_fullscreen(_width, _height: Integer; _format: IPTCFormat);
begin
  FPrimary := TPTCSurface.Create(_width, _height, _format);
end;

procedure TTextFX2Console.internal_open_fullscreen_finish(_pages: Integer);
begin
end;

procedure TTextFX2Console.internal_post_open_setup;
begin
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  FKeyboard := TDosKeyboard.Create;
  FMouse := TDosMouse.Create(FPrimary.width, FPrimary.height);
  FEventQueue := TEventQueue.Create;

  { temporary platform dependent information fudge }
  FInformation := 'dos version x.xx.x, TextFX2, ...';

  { set open flag }
  FOpen := True;
end;

procedure TTextFX2Console.internal_reset;
begin
  FreeAndNil(FPrimary);
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
end;

procedure TTextFX2Console.internal_close;
begin
  FreeAndNil(FPrimary);
  FreeAndNil(F160x100buffer);
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  set80x25;
end;

procedure TTextFX2Console.HandleEvents;
begin
  FKeyboard.GetPendingEvents(FEventQueue);
  FMouse.GetPendingEvents(FEventQueue);
end;

function TTextFX2Console.NextEvent(out event: IPTCEvent; wait: Boolean; const EventMask: TPTCEventMask): Boolean;
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

function TTextFX2Console.PeekEvent(wait: Boolean; const EventMask: TPTCEventMask): IPTCEvent;
begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(EventMask);
  until (not Wait) or (Result <> Nil);
end;

procedure TTextFX2Console.check_open;
begin
  if not FOpen then
    raise TPTCError.Create('console is not open');
end;

procedure TTextFX2Console.check_unlocked;
begin
  if FLocked then
    raise TPTCError.Create('console is not unlocked');
end;
