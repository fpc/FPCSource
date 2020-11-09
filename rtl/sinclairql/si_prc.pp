{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by Karoly Balogh

    System Entry point for the Sinclair QL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit si_prc;

interface

implementation

{$i qdosfuncs.inc}

var
  stacktop: pointer;
  stackorig: pointer;
  setjmpbuf: jmp_buf;
  stklen: longint; external name '__stklen';
  binstart: pointer; external name '_stext';
  binend: pointer; external name '_etext';


procedure PascalMain; external name 'PASCALMAIN';

{ this function must be the first in this unit which contains code }
{$OPTIMIZATION OFF}
function _FPC_proc_start: longint; cdecl; public name '_start';
var
  newstack: pointer;
begin
  _FPC_proc_start:=0;
  asm
    move.l d7,-(sp)
    { relocation code }

    { get our actual position in RAM }
    lea.l binstart(pc),a0
    move.l a0,d0
    { get an offset to the end of the binary. this depends on the
      fact that at this point the binary is not relocated yet }
    lea.l binend,a1
    add.l d0,a1

    { first item in the relocation table is the number of relocs }
    move.l (a1),d7
    beq @noreloc

    { zero out the number of relocs in RAM,  so if our code is
      called again, without reload, it won't relocate itself twice }
    move.l #0,(a1)+
@relocloop:
    { we read the offsets and relocate them }
    move.l (a1)+,d1
    add.l d0,(a0,d1)
    subq.l #1,d7
    bne @relocloop

@noreloc:
    move.l (sp)+,d7

    { save the original stack pointer }
    move.l a7,stackorig
  end;

  newstack:=mt_alchp(stklen,nil,-1);
  if not assigned(newstack) then
    _FPC_proc_start:=ERR_OM
  else
    begin
      asm
        move.l newstack,sp
      end;
      if setjmp(setjmpbuf) = 0 then
        PascalMain;
      asm
        move.l stackorig,sp
      end;
      mt_rechp(newstack);
   end;
end;

procedure _FPC_proc_halt(_ExitCode: longint); public name '_haltproc';
begin
  longjmp(setjmpbuf,1); 
end;


end.
