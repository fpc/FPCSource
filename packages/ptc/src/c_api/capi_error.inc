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

function ptc_error_create(message: string): TPTC_ERROR;
begin
  try
    ptc_error_create := TPTC_ERROR(TPTCError.Create(message));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_error_create := nil;
    end;
  end;
end;

function ptc_error_create_composite(message: string; error: TPTC_ERROR): TPTC_ERROR;
begin
  try
    ptc_error_create_composite := TPTC_ERROR(TPTCError.Create(message, TPTCError(error)));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_error_create_composite := nil;
    end;
  end;
end;

procedure ptc_error_destroy(obj: TPTC_ERROR);
begin
  if obj = nil then
    exit;
  try
    TPTCError(obj).Destroy;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

procedure ptc_error_report(obj: TPTC_ERROR);
begin
  try
    TPTCError(obj).report;
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_error_message(obj: TPTC_ERROR): string;
begin
  try
    ptc_error_message := TPTCError(obj).message;
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_error_message := '';
    end;
  end;
end;

procedure ptc_error_assign(obj, error: TPTC_ERROR);
begin
  try
    TPTCError(obj).Assign(TPTCError(error));
  except
    on error: TPTCError do
      ptc_exception_handle(error);
  end;
end;

function ptc_error_equals(obj, error: TPTC_ERROR): Boolean;
begin
  try
    ptc_error_equals := TPTCError(obj).Equals(TPTCError(error));
  except
    on error: TPTCError do
    begin
      ptc_exception_handle(error);
      ptc_error_equals := False;
    end;
  end;
end;

procedure ptc_error_handler(handler: TPTC_ERROR_HANDLER);
begin
  ptc_exception_handler(handler);
end;
