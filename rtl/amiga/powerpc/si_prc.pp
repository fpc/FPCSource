{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    System Entry point for AmigaOS4/PowerPC, Pascal only programs

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
  amigaos4_signature: dword = 1; public name '__amigaos4__';

var
  AOS_ExecBase: Pointer; public name '_ExecBase';
  IExec: Pointer; public name '_IExec';
  StkLen: LongInt; external name '__stklen';
  StackCookie: LongInt; external name '__stack_cookie';
  sysinit_jmpbuf: jmp_buf;
  ExitCode: LongInt;

{ the definitions in there need AOS_Execbase and IExec }
{$include execd.inc}
{$include execf.inc}

procedure PascalMain; external name 'PASCALMAIN';


{ this function must be the first in this unit which contains code }
{ apparently, the third argument contains the IExec on entry (KB) }
function _FPC_proc_start(arg0: pointer; arg1: pointer; argIExec: POS4Interface): longint; cdecl; public name '_start';
begin
  IExec:=argIExec;
  AOS_ExecBase:=argIExec^.Data.LibBase;
  amigaos4_signature:=1;   { Hack: prevent section gc to remove this, until VLink has a fix (KB) }

  { The StackCookie check is only here so the symbol is referenced and
    doesn't get striped out }
  if StackCookie > 0 then
    if setjmp(sysinit_jmpbuf) = 0 then
      PascalMain;

  _FPC_proc_start:=ExitCode;
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
  ExitCode:=_ExitCode;
  longjmp(sysinit_jmpbuf,1);
end;


end.
