{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by contributors of the Free Pascal Compiler

    Low level file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

function do_isdevice(handle:thandle):boolean;
begin
  //do_isdevice:=(handle = StdInputHandle) or (handle = StdOutputHandle) or (handle = StdErrorHandle);
end;


procedure do_close(h : thandle);
begin
  if do_isdevice(h) then exit;
  
//  CloseHandle(h);
end;


procedure do_erase(p : pchar);
begin

end;


procedure do_rename(p1,p2 : pchar);
begin

end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
begin

end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
begin

end;


function do_filepos(handle : thandle) : Int64;
begin

end;


procedure do_seek(handle:thandle;pos : Int64);
begin

end;


function do_seekend(handle:thandle):Int64;
begin

end;


function do_filesize(handle : thandle) : Int64;
begin

end;


procedure do_truncate (handle:thandle;pos:Int64);
begin

end;


procedure do_open(var f;p:pchar;flags:longint);
begin

end;
