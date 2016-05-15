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


{*****************************************************************************
                              Heap Management
*****************************************************************************}

function SysOSAlloc (size: ptruint): pointer;
var
  regs : Registers;
  nb_para : longint;
begin
{$ifdef DEBUG_TINY_HEAP}
  writeln('SysOSAlloc called size=',size);
{$endif}
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  regs.ax:=$4800;
  nb_para:=size div 16;
  if nb_para > $ffff then
    result:=nil
  else
    begin
      regs.bx:=nb_para;
      msdos(regs);
      if (regs.Flags and fCarry) <> 0 then
        begin
{$ifdef DEBUG_TINY_HEAP}
          writeln('SysOSAlloc failed, err = ',regs.AX);
{$endif}
          GetInOutRes(regs.AX);
  Result := nil;
        end
      else
        begin
          result:=ptr(regs.ax,0);
{$ifdef DEBUG_TINY_HEAP}
          writeln('SysOSAlloc returned= $',hexstr(seg(result),4),':$',hexstr(ofs(result),4));
{$endif}
        end;
    end;
{$else not DATA_FAR}
  Result := nil;
{$endif not DATA_FAR}
end;

procedure SysOSFree(p: pointer; size: ptruint);
begin
end;
