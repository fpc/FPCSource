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

function ptc_surface_create(width, height: Integer; format: TPTC_FORMAT): TPTC_SURFACE;
begin
  try
    ptc_surface_create := TPTC_SURFACE(TPTCSurface.Create(width, height, TPTCFormat(format)));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_create := nil;
    end;
  end;
end;

procedure ptc_surface_destroy(obj: TPTC_SURFACE);
begin
  if obj = nil then
    exit;
  try
    TPTCBaseSurface(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_copy(obj: TPTC_SURFACE; surface: TPTC_SURFACE);
begin
  try
    TPTCBaseSurface(obj).copy(TPTCBaseSurface(surface));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_copy_area(obj: TPTC_SURFACE; surface: TPTC_SURFACE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseSurface(obj).copy(TPTCBaseSurface(surface), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_surface_lock(obj: TPTC_SURFACE): Pointer;
begin
  try
    ptc_surface_lock := TPTCBaseSurface(obj).lock;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_lock := nil;
    end;
  end;
end;

procedure ptc_surface_unlock(obj: TPTC_SURFACE);
begin
  try
    TPTCBaseSurface(obj).unlock;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_load(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseSurface(obj).load(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_load_area(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseSurface(obj).load(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_save(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseSurface(obj).save(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_save_area(obj: TPTC_SURFACE; pixels: Pointer; width, height, pitch: Integer; format: TPTC_FORMAT; palette: TPTC_PALETTE; source, destination: TPTC_AREA);
begin
  try
    TPTCBaseSurface(obj).save(pixels, width, height, pitch, TPTCFormat(format), TPTCPalette(palette), TPTCArea(source), TPTCArea(destination));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_clear(obj: TPTC_SURFACE);
begin
  try
    TPTCBaseSurface(obj).clear;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_clear_color(obj: TPTC_SURFACE; color: TPTC_COLOR);
begin
  try
    TPTCBaseSurface(obj).clear(TPTCColor(color));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_clear_color_area(obj: TPTC_SURFACE; color: TPTC_COLOR; area: TPTC_AREA);
begin
  try
    TPTCBaseSurface(obj).clear(TPTCColor(color), TPTCArea(area));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_surface_palette_set(obj: TPTC_SURFACE; palette: TPTC_PALETTE);
begin
  try
    TPTCBaseSurface(obj).palette(TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_surface_palette_get(obj: TPTC_SURFACE): TPTC_PALETTE;
begin
  try
    ptc_surface_palette_get := TPTC_PALETTE(TPTCBaseSurface(obj).palette);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_palette_get := nil;
    end;
  end;
end;

procedure ptc_surface_clip_set(obj: TPTC_SURFACE; area: TPTC_AREA);
begin
  try
    TPTCBaseSurface(obj).clip(TPTCArea(area));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_surface_width(obj: TPTC_SURFACE): Integer;
begin
  try
    ptc_surface_width := TPTCBaseSurface(obj).width;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_width := 0;
    end;
  end;
end;

function ptc_surface_height(obj: TPTC_SURFACE): Integer;
begin
  try
    ptc_surface_height := TPTCBaseSurface(obj).height;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_height := 0;
    end;
  end;
end;

function ptc_surface_pitch(obj: TPTC_SURFACE): Integer;
begin
  try
    ptc_surface_pitch := TPTCBaseSurface(obj).pitch;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_pitch := 0;
    end;
  end;
end;

function ptc_surface_area(obj: TPTC_SURFACE): TPTC_AREA;
begin
  try
    ptc_surface_area := TPTC_AREA(TPTCBaseSurface(obj).area);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_area := nil;
    end;
  end;
end;

function ptc_surface_clip(obj: TPTC_SURFACE): TPTC_AREA;
begin
  try
    ptc_surface_clip := TPTC_AREA(TPTCBaseSurface(obj).clip);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_clip := nil;
    end;
  end;
end;

function ptc_surface_format(obj: TPTC_SURFACE): TPTC_FORMAT;
begin
  try
    ptc_surface_format := TPTC_FORMAT(TPTCBaseSurface(obj).format);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_format := nil;
    end;
  end;
end;

function ptc_surface_option(obj: TPTC_SURFACE; _option: string): Boolean;
begin
  try
    ptc_surface_option := TPTCBaseSurface(obj).option(_option);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_surface_option := False;
    end;
  end;
end;
