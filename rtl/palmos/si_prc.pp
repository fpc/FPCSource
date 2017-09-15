{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team

    System Entry point for PalmOS, Pascal only programs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

implementation

{$i palmos.inc}

var
  appInfo: SysAppInfoPtr; public name '__appInfo';
  StkLen: LongInt; external name '__stklen';
  sysinit_jmpbuf: jmp_buf;
  ExitCode: LongInt;


procedure PascalMain; external name 'PASCALMAIN';


{ this function must be the first in this unit which contains code }
function _FPC_proc_start: longint; cdecl; public name '_start';
var
  prevGlobals: Pointer;
  globalsPtr: Pointer;
begin
  if SysAppStartup(appInfo, prevGlobals, globalsPtr) <> 0 then
    begin
      SndPlaySystemSound(sndError);
      exit(-1);
    end;

  if setjmp(sysinit_jmpbuf) = 0 then
    PascalMain;

  SysAppExit(appInfo, prevGlobals, globalsPtr);
  _FPC_proc_start:=ExitCode;
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  ExitCode:=_ExitCode;
  longjmp(sysinit_jmpbuf,1);
end;

end.
