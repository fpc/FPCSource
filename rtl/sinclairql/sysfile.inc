{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by Free Pascal development team

    Low level file functions for the Sinclair QL

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
  do_write:=-1;
end;


function do_read(h: longint; addr: pointer; len: longint) : longint;
begin
  do_read:=-1;
end;


function do_filepos(handle: longint) : longint;
begin
  do_filepos:=-1;
end;


procedure do_seek(handle, pos: longint);
begin
end;


function do_seekend(handle: longint):longint;
begin
  do_seekend:=-1;
end;


function do_filesize(handle : THandle) : longint;
begin
  do_filesize:=-1;
end;


{ truncate at a given position }
procedure do_truncate(handle, pos: longint);
begin
end;


procedure do_open(var f;p:pchar;flags:longint; pchangeable: boolean);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
begin
end;


function do_isdevice(handle: thandle): boolean;
begin
  do_isdevice:=false;
end;
