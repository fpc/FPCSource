{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    System Entry point for Human 68k (Sharp X68000)

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
  stacktop: pointer; public name '__stktop';
  stklen: longint; external name '__stklen';


procedure PascalMain; external name 'PASCALMAIN';


{ this function must be the first in this unit which contains code }
{$OPTIMIZATION OFF}
procedure _FPC_proc_start; cdecl; public name '_start';
begin
  PASCALMAIN;
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; assembler public name '_haltproc';
asm
  dc.w $ff00  { _EXIT }
end;


end.
