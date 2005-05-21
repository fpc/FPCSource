{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:

    A simple unit that helps to build array of longint.
    Uses array of const so don't forget to use
    $mode objfpc.

    05 Nov 2002.

    nils.sjoholm@mailbox.swipnet.se
}

unit longarray;

{$mode objfpc}

interface

function readinlongs(const args : array of const): pointer;

implementation

uses pastoc;

var
  argarray : array [0..20] of longint;

function readinlongs(const args : array of const): pointer;
var
   i : longint;

begin

    for i := 0 to High(args) do begin
        case args[i].vtype of
            vtinteger : argarray[i] := longint(args[i].vinteger);
            vtpchar   : argarray[i] := longint(args[i].vpchar);
            vtchar    : argarray[i] := longint(args[i].vchar);
            vtpointer : argarray[i] := longint(args[i].vpointer);
            vtstring  : argarray[i] := longint(pas2c(args[i].vstring^));
            vtboolean : argarray[i] := longint(byte(args[i].vboolean));
        end;
    end;
    readinlongs := @argarray;
end;

end.
