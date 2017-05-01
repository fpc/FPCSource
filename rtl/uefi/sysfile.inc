{
    Basic UEFI definitions

    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Olivier Coursière

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
  //if handle = SysTable.ConOut then
  // for now, no file system support under UEFI : everything is a device...
  // It is required to enable calls to do_write when using WriteLn.
    do_isdevice := true
  //else
    //do_isdevice := false;
  //sysTable.ConOut^.OutputString(handle, 'do_isdevice' + #13#10);
end;

procedure do_close(h : thandle);
begin
end;

procedure do_erase(p : pwidechar; pchangeable: boolean);
begin
end;

procedure do_rename(p1,p2 : pwidechar; p1changeable, p2changeable: boolean);
begin
end;

function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
  p : PChar;
  s : unicodestring;
begin
  //sysTable.ConOut^.OutputString(SysTable.ConOut, 'do_write' + #13#10);
  GetMem(p, len + 1);
  Move(addr^, p^, len);
  p[len] := #0;
  s := UTF8ToAnsi(p);
  case h of
  	//0 : (* Not applicable : input only...*);
  	1 : sysTable.ConOut^.OutputString(SysTable.ConOut, PChar16(s));
  	2 : sysTable.ConOut^.OutputString(SysTable.StdErr, PChar16(s));
  	else
  	  // TODO : writing to files...
  	  ;
  end;
  
  FreeMem(p);
  do_write := len;
end;

function do_read(h: thandle; addr: pointer; len: longint): longint;
begin
end;

function do_filepos(handle : thandle) : Int64;
begin
end;

procedure do_seek(handle: thandle; pos: Int64);
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

procedure do_open(var f;p:pwidechar;flags:longint; pchangeable: boolean);
begin
end;
