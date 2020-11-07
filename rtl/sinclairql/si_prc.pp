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

var
  procdesc: PPD; public name '__base';
  stacktop: pointer;
  stklen: longint; external name '__stklen';


procedure PascalMain; external name 'PASCALMAIN';


{ this function must be the first in this unit which contains code }
{$OPTIMIZATION OFF}
procedure _FPC_proc_start(pd: PPD); cdecl; public name '_start';
begin
end;

procedure _FPC_proc_halt(_ExitCode: longint); cdecl; public name '_haltproc';
begin
end;


end.
