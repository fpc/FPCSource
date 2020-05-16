{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    System Entry point for MorphOS, Pascal only programs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

implementation

const
  abox_signature: dword = 1; public name '__abox__';

var
  MOS_ExecBase: Pointer; public name '_ExecBase';
  realExecBase: Pointer absolute $4;
  StkLen: LongInt; external name '__stklen';
  sysinit_jmpbuf: jmp_buf;
  ExitCode: LongInt;

{ the definitions in there need MOS_Execbase }
{$include execd.inc}
{$include execf.inc}

procedure PascalMainEntry; cdecl; forward;

{ this function must be the first in this unit which contains code }
function _FPC_proc_start: longint; cdecl; public name '_start';
var
  sst: TStackSwapStruct;
  newStack: Pointer;
  newStackAligned: Pointer;
begin
  // prevent removal of the __abox__ symbol by --gc-sections
  abox_signature := 1;
  //
  MOS_ExecBase:=realExecBase;

  newStack:=AllocVecTaskPooled(StkLen+16);
  newStackAligned:=align(newStack,16);

  sst.stk_Lower:=newStackAligned;
  sst.stk_Upper:=newStackAligned+StkLen;
  sst.stk_Pointer:=newStackAligned+StkLen;

  NewPPCStackSwap(@sst,@PascalMainEntry,nil);

  FreeVecTaskPooled(newStack);
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
