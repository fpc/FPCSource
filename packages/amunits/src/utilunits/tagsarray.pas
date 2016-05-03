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

unit tagsarray;
{$mode objfpc}{$H+}

interface

uses
  Exec, Utility;

type
  TTagsList = array of ttagitem;
  PMyTags = ^TTagsList;


function ReadInTags(const Args: array of const): PTagItem;
procedure AddTags(var Taglist: TTagsList; const Args: array of const);
function GetTagPtr(var TagList: TTagsList): PTagItem;

implementation

var
  MyTags: PMyTags;

procedure AddTags(var Taglist: TTagsList; const Args: array of const);
var
  i: PtrInt;
  ii: PtrInt;
begin
  ii := Length(TagList);
  SetLength(TagList, Length(TagList) + (Length(args) DIV 2));
  for i := 0 to High(args) do
  begin
    if (not Odd(i)) then
    begin
      TagList[ii].ti_tag := PtrInt(Args[i].vinteger);
    end else
    begin
      case Args[i].vtype of
        vtinteger : TagList[ii].ti_data := PtrInt(Args[i].vinteger);
        vtboolean : TagList[ii].ti_data := PtrInt(byte(Args[i].vboolean));
        vtpchar   : TagList[ii].ti_data := PtrInt(Args[i].vpchar);
        vtchar    : TagList[ii].ti_data := PtrInt(Args[i].vchar);
        vtstring  : TagList[ii].ti_data := PtrInt(PChar(string(Args[i].vstring^)));
        vtpointer : TagList[ii].ti_data := PtrInt(Args[i].vpointer);
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

function ReadInTags(const Args: array of const): PTagItem;
var
  i: PtrInt;
  ii: PtrInt;
begin
  ii := 0;
  SetLength(MyTags^, (Length(Args) div 2) + 4); // some more at the end
  for i := 0 to High(Args) do
  begin
    if not Odd(i) then
    begin
      mytags^[ii].ti_tag := PtrInt(Args[i].vinteger);
    end else
    begin
      case Args[i].vtype of
        vtinteger: mytags^[ii].ti_data := PtrInt(Args[i].vinteger);
        vtboolean: mytags^[ii].ti_data := PtrInt(Byte(Args[i].vboolean));
        vtpchar: mytags^[ii].ti_data := PtrInt(Args[i].vpchar);
        vtchar: mytags^[ii].ti_data := PtrInt(Args[i].vchar);
        vtstring: mytags^[ii].ti_data := PtrInt(PChar(string(Args[i].vstring^)));
        vtpointer: mytags^[ii].ti_data := PtrInt(Args[i].vpointer);
      end;
      Inc(ii);
    end;
  end;
  Inc(ii);
  // Add additional TAG_DONE (if user forget)
  mytags^[ii].ti_tag := TAG_DONE;
  mytags^[ii].ti_data := 0;
  // return the pointer
  ReadInTags := @(MyTags^[0]);
end;

initialization
  New(MyTags);
  SetLength(MyTags^, 200);
finalization
  SetLength(MyTags^, 0);
  Dispose(MyTags);
end.
