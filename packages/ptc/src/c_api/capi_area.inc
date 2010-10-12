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

function ptc_area_create(left, top, right, bottom: Integer): TPTC_AREA;
begin
  try
    ptc_area_create := TPTC_AREA(TPTCArea.Create(left, top, right, bottom));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_create := nil;
    end;
  end;
end;

procedure ptc_area_destroy(obj: TPTC_AREA);
begin
  if obj = nil then
    exit;
  try
    TPTCArea(obj).Destroy;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
    end;
  end;
end;

function ptc_area_left(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_left := TPTCArea(obj).left;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_left := 0;
    end;
  end;
end;

function ptc_area_top(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_top := TPTCArea(obj).top;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_top := 0;
    end;
  end;
end;

function ptc_area_right(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_right := TPTCArea(obj).right;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_right := 0;
    end;
  end;
end;

function ptc_area_bottom(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_bottom := TPTCArea(obj).bottom;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_bottom := 0;
    end;
  end;
end;

function ptc_area_width(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_width := TPTCArea(obj).width;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_width := 0;
    end;
  end;
end;

function ptc_area_height(obj: TPTC_AREA): Integer;
begin
  try
    ptc_area_height := TPTCArea(obj).height;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_height := 0;
    end;
  end;
end;

procedure ptc_area_assign(obj, area: TPTC_AREA);
begin
  try
    TPTCArea(obj).Assign(TPTCArea(area));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
    end;
  end;
end;

function ptc_area_equals(obj, area: TPTC_AREA): Boolean;
begin
  try
    ptc_area_equals := TPTCArea(obj).Equals(TPTCArea(area));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_area_equals := False;
    end;
  end;
end;
