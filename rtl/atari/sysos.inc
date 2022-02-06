{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$include xbios.inc}
{$include gemdos.inc}

procedure Error2InOutRes(errno: longint);
begin
  if (errno <= -2) and (errno >= -11) then
    InOutRes:=150-errno
  else
    begin
      case errno of
        EINVFN : InOutRes:=1;
        EFILNF : InOutRes:=2;
        EPTHNF : InOutRes:=3;
        ENHNDL : InOutRes:=4;
        EACCDN : InOutRes:=5;
        EIHNDL : InOutRes:=6;
        ENSMEM,EGSBF : InOutRes:=8;
        EIMBA : InOutRes:=9;
        EDRIVE : InOutRes:=15;
        EPLFMT,EINTRN,ERANGE : InOutRes:=153;
        -15 : InOutRes:=151;
        -13 : InOutRes:=150;
      else
        InOutRes := word(errno);
      end;
    end;
end;

var
  stackpointer_on_entry: pointer; external name '__stktop';

function StackTop: Pointer;
begin
  StackTop:=stackpointer_on_entry;
end;

var
  basepage: PPD; external name '__base';

function SysBackTraceStr (Addr: CodePointer): ShortString;
begin
  if (addr<basepage^.p_tbase) or (addr>pointer(basepage^.p_tbase+basepage^.p_tlen)) then
    SysBackTraceStr:='  Addr $'+hexstr(addr)
  else
    SysBackTraceStr:='  Offs $'+hexstr(pointer(addr-basepage^.p_tbase));
end;
