{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh

    get9.library interface unit for MorphOS/PowerPC

    Free Pascal MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit get9;

interface

uses exec;

var
  Get9Base: Pointer = nil;

const
  GET9NAME : PChar = 'get9.library';

procedure DNetCheck(force: boolean);
SysCall BaseSysV Get9Base 28;

function InitGet9Library : boolean;

implementation

const
  LIBVERSION=1;

function InitGet9Library : boolean;
begin
  InitGet9Library := Assigned(Get9Base);
end;

initialization
  Get9Base := OpenLibrary(GET9NAME,LIBVERSION);
finalization
  if Assigned(Get9Base) then
    CloseLibrary(PLibrary(Get9Base));
end.
