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

function ptc_key_create(code: Integer; alt, shift, control: Boolean): TPTC_KEY;
begin
  try
    ptc_key_create := TPTC_KEY(TPTCKeyEvent.Create(code, alt, shift, control));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_create := nil;
    end;
  end;
end;

procedure ptc_key_destroy(obj: TPTC_KEY);
begin
  if obj = nil then
    exit;
  try
    TPTCKeyEvent(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_key_code(obj: TPTC_KEY): Integer;
begin
  try
    ptc_key_code := Integer(TPTCKeyEvent(obj).code);
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_code := 0;
    end;
  end;
end;

function ptc_key_alt(obj: TPTC_KEY): Boolean;
begin
  try
    ptc_key_alt := TPTCKeyEvent(obj).alt;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_alt := False;
    end;
  end;
end;

function ptc_key_shift(obj: TPTC_KEY): Boolean;
begin
  try
    ptc_key_shift := TPTCKeyEvent(obj).shift;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_shift := False;
    end;
  end;
end;

function ptc_key_control(obj: TPTC_KEY): Boolean;
begin
  try
    ptc_key_control := TPTCKeyEvent(obj).control;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_control := False;
    end;
  end;
end;

procedure ptc_key_assign(obj: TPTC_KEY; key: TPTC_KEY);
begin
  try
    TPTCKeyEvent(obj).Assign(TPTCKeyEvent(key));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_key_equals(obj: TPTC_KEY; key: TPTC_KEY): Boolean;
begin
  try
    ptc_key_equals := TPTCKeyEvent(obj).Equals(TPTCKeyEvent(key));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_key_equals := False;
    end;
  end;
end;
