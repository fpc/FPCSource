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

function ptc_color_create: TPTC_COLOR;
begin
  try
    ptc_color_create := TPTC_COLOR(TPTCColor.Create);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_create := nil;
    end;
  end;
end;

function ptc_color_create_indexed(index: Integer): TPTC_COLOR;
begin
  try
    ptc_color_create_indexed := TPTC_COLOR(TPTCColor.Create(index));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_create_indexed := nil;
    end;
  end;
end;

function ptc_color_create_direct(r, g, b, a: Single): TPTC_COLOR;
begin
  try
    ptc_color_create_direct := TPTC_COLOR(TPTCColor.Create(r, g, b, a));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_create_direct := nil;
    end;
  end;
end;

procedure ptc_color_destroy(obj: TPTC_COLOR);
begin
  if obj = nil then
    exit;
  try
    TPTCColor(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_color_index(obj: TPTC_COLOR): Integer;
begin
  try
    ptc_color_index := TPTCColor(obj).index;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_index := 0;
    end;
  end;
end;

function ptc_color_r(obj: TPTC_COLOR): Single;
begin
  try
    ptc_color_r := TPTCColor(obj).r;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_r := 0;
    end;
  end;
end;

function ptc_color_g(obj: TPTC_COLOR): Single;
begin
  try
    ptc_color_g := TPTCColor(obj).g;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_g := 0;
    end;
  end;
end;

function ptc_color_b(obj: TPTC_COLOR): Single;
begin
  try
    ptc_color_b := TPTCColor(obj).b;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_b := 0;
    end;
  end;
end;

function ptc_color_a(obj: TPTC_COLOR): Single;
begin
  try
    ptc_color_a := TPTCColor(obj).a;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_a := 0;
    end;
  end;
end;

function ptc_color_direct(obj: TPTC_COLOR): Boolean;
begin
  try
    ptc_color_direct := TPTCColor(obj).direct;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_direct := False;
    end;
  end;
end;

function ptc_color_indexed(obj: TPTC_COLOR): Boolean;
begin
  try
    ptc_color_indexed := TPTCColor(obj).indexed;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_indexed := False;
    end;
  end;
end;

procedure ptc_color_assign(obj, color: TPTC_COLOR);
begin
  try
    TPTCColor(obj).Assign(TPTCColor(color));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_color_equals(obj, color: TPTC_COLOR): Boolean;
begin
  try
    ptc_color_equals := TPTCColor(obj).Equals(TPTCColor(color));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_color_equals := False;
    end;
  end;
end;
