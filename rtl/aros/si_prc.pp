{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team

    System Entry point for AROS, Pascal only programs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

implementation

var
  AOS_ExecBase: Pointer; public name '_ExecBase';
  StkLen: LongInt; external name '__stklen';
  sysinit_jmpbuf: jmp_buf;
  ExitCode: LongInt;

{$include execd.inc}
{$include execf.inc}

procedure PascalMainEntry; cdecl; forward;

{ this function must be the first in this unit which contains code }
function _FPC_proc_start(argv: pointer; argc: ptrint; argExecBase: Pointer): longint; cdecl; public name '_start';
var
  sst: TStackSwapStruct;
  ssp: TStackSwapArgs;
  newStack: Pointer;
  newStackAligned: Pointer;
  task: PTask;
begin
  AOS_ExecBase:=argExecBase;
  newStack:=nil;
  newStackAligned:=nil;

  task:=FindTask(nil);
  if (task^.tc_SPUpper-task^.tc_SPLower < StkLen) then
    begin
      newStack:=AllocVec(StkLen+16, MEMF_ANY);
      newStackAligned:=align(newStack,16);

      sst.stk_Lower:=newStackAligned;
      sst.stk_Upper:=newStackAligned+StkLen;
      sst.stk_Pointer:=newStackAligned+StkLen;

      FillChar(ssp,sizeof(ssp),0);
      NewStackSwap(@sst,@PascalMainEntry,@ssp);

      FreeVec(newStack);
    end
  else
    PascalMainEntry;

  _FPC_proc_start:=ExitCode;
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  ExitCode:=_ExitCode;
  longjmp(sysinit_jmpbuf,1);
end;


procedure PascalMain; external name 'PASCALMAIN';

procedure PascalMainEntry; cdecl;
begin
  if setjmp(sysinit_jmpbuf) = 0 then
    PascalMain;
end;


end.
