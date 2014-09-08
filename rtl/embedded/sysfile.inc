{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level file functions
    GBA does not have any drive, so no file handling is needed.
    Copyright (c) 2006 by Francesco Lombardi

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                        Low level File Routines
               All these functions can set InOutRes on errors
****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin

end;

procedure do_erase(p : pchar; pchangeable: boolean);
begin
end;

procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
begin
end;

function do_write(h: longint; addr: pointer; len: longint) : longint;
begin
  result := -1;
end;

function do_read(h: longint; addr: pointer; len: longint) : longint;
begin
  result := -1;
end;

function do_filepos(handle: longint) : longint;
begin
  result := -1;
end;

procedure do_seek(handle, pos: longint);
begin
end;

function do_seekend(handle: longint):longint;
begin
  result := -1;
end;

function do_filesize(handle : longint) : longint;
begin
  result := -1;
end;

{ truncate at a given position }
procedure do_truncate(handle, pos: longint);
begin
end;

procedure do_open(var f;p:PFileTextRecChar;flags:longint; pchangeable: boolean);
begin
end;

function do_isdevice(handle: longint): boolean;
begin
  result := false;
end;

