{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2010  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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

function ptc_console_create: TPTC_CONSOLE;
begin
  try
    ptc_console_create := TPTC_CONSOLE(TPTCConsole.Create);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_create := nil;
    end;
  end;
end;

procedure ptc_console_destroy(obj: TPTC_CONSOLE);
begin
  if obj = nil then
    exit;
  try
    TPTCBaseConsole(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_configure(obj: TPTC_CONSOLE; _file: String);
begin
  try
    TPTCBaseConsole(obj).configure(_file);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_console_option(obj: TPTC_CONSOLE; _option: String): Boolean;
begin
  try
    ptc_console_option := TPTCBaseConsole(obj).option(_option);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_option := False;
    end;
  end;
end;

function ptc_console_mode(obj: TPTC_CONSOLE; index: Integer): TPTC_MODE;
begin
  try
    ptc_console_mode := TPTC_MODE(TPTCBaseConsole(obj).modes[index]);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_mode := nil;
    end;
  end;
end;

procedure ptc_console_open(obj: TPTC_CONSOLE; title: string; pages: Integer);
begin
  try
    TPTCBaseConsole(obj).open(title, pages);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_open_format(obj: TPTC_CONSOLE; title: string; format: TPTC_FORMAT; pages: Integer);
begin
  try
    TPTCBaseConsole(obj).open(title, TPTCFormat(format), pages);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_open_resolution(obj: TPTC_CONSOLE; title: string; width, height: Integer; format: TPTC_FORMAT; pages: Integer);
begin
  try
    TPTCBaseConsole(obj).open(title, width, height, TPTCFormat(format), pages);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_open_mode(obj: TPTC_CONSOLE; title: string; mode: TPTC_MODE; pages: Integer);
begin
  try
    TPTCBaseConsole(obj).open(title, TPTCMode(mode), pages);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_close(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).close;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_flush(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).flush;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_finish(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).finish;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_update(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).update;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_update_area(obj: TPTC_CONSOLE; area: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).update(TPTCArea(area));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_console_key(obj: TPTC_CONSOLE): Boolean;
begin
  try
    ptc_console_key := TPTCBaseConsole(obj).key;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_key := False;
    end;
  end;
end;

procedure ptc_console_read(obj: TPTC_CONSOLE; key: TPTC_KEY);
var
  tmp: TPTCKeyEvent;
begin
  try
    tmp := TPTCBaseConsole(obj).read;
    try
      TPTCKeyEvent(key).Assign(tmp);
    finally
      tmp.Destroy;
    end;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_copy(obj: TPTC_CONSOLE; surface: TPTC_SURFACE);
begin
  try
    TPTCBaseConsole(obj).copy(TPTCBaseSurface(surface));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_copy_area(obj: TPTC_CONSOLE; surface: TPTC_SURFACE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).copy(TPTCBaseSurface(surface), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_console_lock(obj: TPTC_CONSOLE): Pointer;
begin
  try
    ptc_console_lock := TPTCBaseConsole(obj).lock;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_lock := nil;
    end;
  end;
end;

procedure ptc_console_unlock(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).unlock;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_load(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseConsole(obj).load(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_load_area(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).load(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_save(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseConsole(obj).save(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_save_area(obj: TPTC_CONSOLE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).save(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_clear(obj: TPTC_CONSOLE);
begin
  try
    TPTCBaseConsole(obj).clear;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_clear_color(obj: TPTC_CONSOLE; color: TPTC_COLOR);
begin
  try
    TPTCBaseConsole(obj).clear(TPTCColor(color));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_clear_color_area(obj: TPTC_CONSOLE; color: TPTC_COLOR; area: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).clear(TPTCColor(color), TPTCArea(area));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_console_palette_set(obj: TPTC_CONSOLE; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseConsole(obj).palette(TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_console_palette_get(obj: TPTC_CONSOLE): TPTC_PALETTE;
begin
  try
    ptc_console_palette_get := TPTC_PALETTE(TPTCBaseConsole(obj).palette);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_palette_get := nil;
    end;
  end;
end;

procedure ptc_console_clip_set(obj: TPTC_CONSOLE; area: TPTC_AREA);
begin
  try
    TPTCBaseConsole(obj).clip(TPTCArea(area));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_console_width(obj: TPTC_CONSOLE): Integer;
begin
  try
    ptc_console_width := TPTCBaseConsole(obj).width;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_width := 0;
    end;
  end;
end;

function ptc_console_height(obj: TPTC_CONSOLE): Integer;
begin
  try
    ptc_console_height := TPTCBaseConsole(obj).height;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_height := 0;
    end;
  end;
end;

function ptc_console_pages(obj: TPTC_CONSOLE): Integer;
begin
  try
    ptc_console_pages := TPTCBaseConsole(obj).pages;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_pages := 0;
    end;
  end;
end;

function ptc_console_pitch(obj: TPTC_CONSOLE): Integer;
begin
  try
    ptc_console_pitch := TPTCBaseConsole(obj).pitch;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_pitch := 0;
    end;
  end;
end;

function ptc_console_area(obj: TPTC_CONSOLE): TPTC_AREA;
begin
  try
    ptc_console_area := TPTC_AREA(TPTCBaseConsole(obj).area);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_area := nil;
    end;
  end;
end;

function ptc_console_clip(obj: TPTC_CONSOLE): TPTC_AREA;
begin
  try
    ptc_console_clip := TPTC_AREA(TPTCBaseConsole(obj).clip);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_clip := nil;
    end;
  end;
end;

function ptc_console_format(obj: TPTC_CONSOLE): TPTC_FORMAT;
begin
  try
    ptc_console_format := TPTC_FORMAT(TPTCBaseConsole(obj).format);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_format := nil;
    end;
  end;
end;

function ptc_console_name(obj: TPTC_CONSOLE): string;
begin
  try
    ptc_console_name := TPTCBaseConsole(obj).name;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_name := '';
    end;
  end;
end;

function ptc_console_title(obj: TPTC_CONSOLE): string;
begin
  try
    ptc_console_title := TPTCBaseConsole(obj).title;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_title := '';
    end;
  end;
end;

function ptc_console_information(obj: TPTC_CONSOLE): string;
begin
  try
    ptc_console_information := TPTCBaseConsole(obj).information;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_console_information := '';
    end;
  end;
end;
