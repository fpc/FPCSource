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


procedure do_erase(p : pchar);
begin
end;


procedure do_rename(p1,p2 : pchar);
begin
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
begin
  { TODO: add error checking }
  asm
    mov ah, 40h
    mov bx, h
    mov cx, len
    mov dx, addr
    int 21h
  end ['ax','bx','cx','dx'];
  do_write := len;
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

procedure do_open(var f;p:pchar;flags:longint);
begin
end;


function do_isdevice(handle:THandle):boolean;
label
  no_device;
begin
  { TODO: add error checking }
  asm
    mov ax, 4400h
    mov bx, handle
    int 21h
    xor al, al
    test dl, 80h
    jz no_device
    inc al
no_device:
    mov do_isdevice, al
  end ['ax','bx','dx'];
end;
