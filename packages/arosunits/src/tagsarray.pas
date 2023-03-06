{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit tagsarray;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Amiga.Core.Exec, Amiga.Core.Utility;
{$ELSE FPC_DOTTEDUNITS}
uses
  Exec, Utility;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTagsList = array of ttagitem;

procedure AddTags(var Taglist: TTagsList; const Args: array of const);
function GetTagPtr(var TagList: TTagsList): PTagItem;

implementation

procedure AddTags(var Taglist: TTagsList; const Args: array of const);
var
  i: IPTR;
  ii: IPTR;
begin
  ii := Length(TagList);
  SetLength(TagList, Length(TagList) + (Length(args) DIV 2));
  for i := 0 to High(args) do
  begin
    if (not Odd(i)) then
    begin
      TagList[ii].ti_tag := IPTR(Args[i].vinteger);
    end else
    begin
      case Args[i].vtype of
        vtinteger : TagList[ii].ti_data := IPTR(Args[i].vinteger);
        vtboolean : TagList[ii].ti_data := IPTR(byte(Args[i].vboolean));
        vtpchar   : TagList[ii].ti_data := IPTR(Args[i].vpchar);
        vtchar    : TagList[ii].ti_data := IPTR(Args[i].vchar);
        vtstring  : TagList[ii].ti_data := IPTR(PAnsiChar(AnsiString(Args[i].vstring^)));
        vtpointer : TagList[ii].ti_data := IPTR(Args[i].vpointer);
      end;
      inc(ii);
    end;
  end;
end;

function GetTagPtr(var TagList: TTagsList): pTagItem;
begin
  AddTags(TagList, [TAG_END, TAG_END]);
  GetTagPtr := @(TagList[0]);
end;

initialization
finalization
end.
