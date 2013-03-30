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
var
  regs: Registers;
begin
  regs.AH := $40;
  regs.BX := h;
  regs.CX := len;
  regs.DS := DSeg;
  regs.DX := Ofs(addr^);
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
  begin
    GetInOutRes(regs.AX);
    exit(0);
  end;
  do_write := regs.AX;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
var
  regs: Registers;
begin
  regs.AH := $3F;
  regs.BX := h;
  regs.CX := len;
  regs.DS := DSeg;
  regs.DX := Ofs(addr^);
  MsDos(regs);
  if (regs.Flags and FCarry) <> 0 then
  begin
    GetInOutRes(regs.AX);
    exit(0);
  end;
  do_read := regs.AX;
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
var
  regs: Registers;
begin
  regs.AX := $4400;
  regs.BX := handle;
  MsDos(regs);
  do_isdevice := (regs.DL and $80) <> 0;
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;
