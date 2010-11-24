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

function ptc_palette_create: TPTC_PALETTE;
begin
  try
    ptc_palette_create := TPTC_PALETTE(TPTCPalette.Create);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_palette_create := nil;
    end;
  end;
end;

{function ptc_palette_create_data(data: Pint32): TPTC_PALETTE;

begin
  try
    ptc_palette_create_data := TPTC_PALETTE(TPTCPalette.Create(data));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_palette_create_data := nil;
    end;
  end;
End;}

procedure ptc_palette_destroy(obj: TPTC_PALETTE);
begin
  if obj = nil then
    exit;
  try
    TPTCPalette(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_palette_lock(obj: TPTC_PALETTE): Pint32;
begin
  try
    ptc_palette_lock := TPTCPalette(obj).lock;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_palette_lock := nil;
    end;
  end;
end;

procedure ptc_palette_unlock(obj: TPTC_PALETTE);
begin
  try
    TPTCPalette(obj).unlock;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_palette_load(obj: TPTC_PALETTE; data: Pint32);
begin
  try
    TPTCPalette(obj).load(data);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_palette_save(obj: TPTC_PALETTE; data: Pint32);
begin
  try
    TPTCPalette(obj).save(data);
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_palette_data(obj: TPTC_PALETTE): Pint32;
begin
  try
    ptc_palette_data := TPTCPalette(obj).data;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_palette_data := nil;
    end;
  end;
end;

procedure ptc_palette_assign(obj, palette: TPTC_PALETTE);
begin
  try
    TPTCPalette(obj).Assign(TPTCPalette(palette));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_palette_equals(obj, palette: TPTC_PALETTE): Boolean;
begin
  try
    ptc_palette_equals := TPTCPalette(obj).Equals(TPTCPalette(palette));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_palette_equals := False;
    end;
  end;
end;
