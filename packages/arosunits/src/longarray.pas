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
{$mode objfpc}{$H+}

interface
uses
  Exec;

type
  PArgList = ^TArgList;
  TArgList = array of IPTR;

procedure AddArguments(var ArgList: TArgList; const Args: array of const);
function GetArgPtr(var ArgList: TArgList): Pointer;

implementation

procedure AddArguments(var ArgList: TArgList; const Args: array of const);
var
  i: Integer;
  Offset: Integer;
begin
  Offset := Length(ArgList);
  SetLength(ArgList, Length(ArgList) + Length(Args));
  for i := 0 to High(Args) do
  begin
    case Args[i].vtype of
      vtinteger: ArgList[Offset + i] := IPTR(Args[i].vinteger);
      vtpchar: ArgList[Offset + i] := IPTR(Args[i].vpchar);
      vtchar: ArgList[Offset + i] := IPTR(Args[i].vchar);
      vtpointer: ArgList[Offset + i] := IPTR(Args[i].vpointer);
      vtstring: ArgList[Offset + i] := IPTR(PChar(string(Args[i].vstring^)));
      vtboolean: ArgList[Offset + i] := IPTR(Byte(Args[i].vboolean));
    end;
  end;
end;

function GetArgPtr(var ArgList: TArgList): Pointer;
var
  Idx: Integer;
begin
  Idx := Length(ArgList);
  SetLength(ArgList, Idx + 1);
  ArgList[Idx] := 0;
  Result := @(ArgList[0]);
end;

end.

