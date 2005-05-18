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
  Get9Base: Pointer;

const
  GET9NAME : PChar = 'get9.library';

procedure DNetCheck(force: boolean);
SysCall BaseSysV Get9Base 28;

function InitGet9Library : boolean;

implementation

var
  Get9_exit : Pointer;

const
  LIBVERSION=1;

procedure CloseGet9Library;
begin
  ExitProc := Get9_exit;
  if Get9Base <> nil then begin
    CloseLibrary(PLibrary(Get9Base));
    Get9Base := nil;
  end;
end;

function InitGet9Library : boolean;
begin
  Get9Base := nil;
  Get9Base := OpenLibrary(GET9NAME,LIBVERSION);
  if Get9Base <> nil then begin
    Get9_exit := ExitProc;
    ExitProc := @CloseGet9Library;
    InitGet9Library:=True;
  end else begin
    InitGet9Library:=False;
  end;
end;

begin
end.
