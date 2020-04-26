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

procedure do_close(handle : thandle);inline;
begin
end;


procedure do_erase(p : pchar; pchangeable: boolean);inline;
begin
end;


procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);inline;
begin
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
begin
  do_write:=len;
  while len>0 do
  begin
    PrintChar(PChar(addr)^);
    Inc(addr);
    Dec(len);
  end;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
var
  ch: Char;
begin
  do_read:=0;
  while len>0 do
  begin
    ch:=ReadKey;
    PrintChar(ch);
    PChar(addr)^:=ch;
    Inc(addr);
    Inc(do_read);
    Dec(len);
    if ch=#13 then
      break;
  end;
end;


function do_filepos(handle : thandle) : longint;inline;
begin
end;


procedure do_seek(handle:thandle;pos : longint);inline;
begin
end;



function do_seekend(handle:thandle):longint;inline;
begin
end;


function do_filesize(handle : thandle) : longint;inline;
begin
end;


{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);inline;
begin
end;

procedure do_open(var f;p:pchar;flags:longint; pchangeable: boolean);inline;
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
begin
end;


function do_isdevice(handle:THandle):boolean;inline;
begin
  do_isdevice:=true;
end;
