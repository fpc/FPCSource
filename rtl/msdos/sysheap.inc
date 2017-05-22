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

{$ifdef DEBUG_TINY_HEAP}
{ Internal structure used by MSDOS }
type
  MCB = packed record
    sig : char;
    psp : word;
    paragraphs : word;
    res : array [0..2] of char;
    exename : array [0..7] of char;
  end;
  PMCB = ^MCB;
{$endif def DEBUG_TINY_HEAP}


function SysOSAlloc (size: ptruint): pointer;
var
  regs : Registers;
  nb_para : longint;
{$ifdef DEBUG_TINY_HEAP}
  p : pmcb;
  i : byte;
{$endif def DEBUG_TINY_HEAP}
begin
{$ifdef DEBUG_TINY_HEAP}
  writeln('SysOSAlloc called size=',size);
{$endif}
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  regs.ax:=$4800;
  nb_para:=size div 16;
  if nb_para > $ffff then
    begin
{$ifdef DEBUG_TINY_HEAP}
      writeln('SysOSAlloc size too big = ',size);
{$endif}
      result:=nil;
    end
  else
    begin
      regs.bx:=nb_para;
      msdos(regs);
      if (regs.Flags and fCarry) <> 0 then
        begin
{$ifdef DEBUG_TINY_HEAP}
          writeln('SysOSAlloc failed, err = ',regs.AX);
{$endif}
          { Do not set InOutRes if ReturnNilIfGrowHeapFails is set }
          if not ReturnNilIfGrowHeapFails then
            GetInOutRes(regs.AX);
          Result := nil;
        end
      else
        begin
          result:=ptr(regs.ax,0);
{$ifdef DEBUG_TINY_HEAP}
          writeln('SysOSAlloc returned= $',hexstr(regs.ax,4),':$0');
          p:=ptr(regs.ax-1,0);
          writeln('Possibly prev MCB: at ',hexstr(p));
          writeln('  sig=',p^.sig);
          writeln('  psp=$',hexstr(p^.psp,4));
          writeln('  paragraphs=',p^.paragraphs);
          if (p^.exename[0]<>#0) then
            begin
              write('  name=');
              for i:=0 to 7 do
                if ord(p^.exename[i])>31 then
                  write(p^.exename[i]);
              writeln;
            end;
          p:=ptr(regs.ax+p^.paragraphs,0);
          writeln('Possibly next MCB: at ',hexstr(p));
          writeln('  sig=',p^.sig);
          writeln('  psp=$',hexstr(p^.psp,4));
          writeln('  paragraphs=',p^.paragraphs);
          if (p^.exename[0]<>#0) then
            begin
              write('  name=');
              for i:=0 to 7 do
                if ord(p^.exename[i])>31 then
                  write(p^.exename[i]);
              writeln;
            end;
{$endif}
        end;
    end;
{$else not DATA_FAR}
{$ifdef DEBUG_TINY_HEAP}
    writeln('SysOSAlloc cannot be used in small data models');
{$endif}
  Result := nil;
{$endif not DATA_FAR}
end;

procedure SysOSFree(p: pointer; size: ptruint);
begin
end;
