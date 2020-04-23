{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Low leve file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                        Low level File Routines
 ****************************************************************************}

procedure do_close(handle : thandle);
begin
end;


procedure do_erase(p : pchar; pchangeable: boolean);
begin
end;


procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
begin
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
begin
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
begin
end;


function do_filepos(handle : thandle) : longint;
begin
end;


procedure do_seek(handle:thandle;pos : longint);
begin
end;



function do_seekend(handle:thandle):longint;
begin
end;


function do_filesize(handle : thandle) : longint;
begin
end;


{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
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


function do_isdevice(handle:THandle):boolean;
begin
end;
