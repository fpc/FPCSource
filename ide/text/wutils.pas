{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WUtils;

interface

uses Objects;

type
     PByteArray = ^TByteArray;
     TByteArray = array[0..65520] of byte;

    PUnsortedStringCollection = ^TUnsortedStringCollection;
    TUnsortedStringCollection = object(TCollection)
      function  At(Index: Integer): PString;
      procedure FreeItem(Item: Pointer); virtual;
    end;

function Min(A,B: longint): longint;
function Max(A,B: longint): longint;

function CharStr(C: char; Count: byte): string;
function Trim(S: string): string;
function UpcaseStr(S: string): string;
function RExpand(S: string; MinLen: byte): string;
function LTrim(S: string): string;
function RTrim(S: string): string;
function IntToStr(L: longint): string;
function StrToInt(S: string): longint;
function GetStr(P: PString): string;

function EatIO: integer;

const LastStrToIntResult : integer = 0;

implementation

function Min(A,B: longint): longint; begin if A<B then Min:=A else Min:=B; end;
function Max(A,B: longint): longint; begin if A>B then Max:=A else Max:=B; end;
function CharStr(C: char; Count: byte): string;
var S: string;
begin S[0]:=chr(Count); if Count>0 then FillChar(S[1],Count,C); CharStr:=S; end;

function UpcaseStr(S: string): string;
var I: integer;
begin
  for I:=1 to length(S) do
      S[I]:=Upcase(S[I]);
  UpcaseStr:=S;
end;

function RExpand(S: string; MinLen: byte): string;
begin
  if length(S)<MinLen then
     S:=S+CharStr(' ',MinLen-length(S));
  RExpand:=S;
end;


function LTrim(S: string): string;
begin
  while copy(S,1,1)=' ' do Delete(S,1,1);
  LTrim:=S;
end;

function RTrim(S: string): string;
begin
  while copy(S,length(S),1)=' ' do Delete(S,length(S),1);
  RTrim:=S;
end;


function Trim(S: string): string;
begin
  Trim:=RTrim(LTrim(S));
end;


function IntToStr(L: longint): string;
var S: string;
begin
  Str(L,S);
  IntToStr:=S;
end;


function StrToInt(S: string): longint;
var L: longint;
    C: integer;
begin
  Val(S,L,C); if C<>0 then L:=-1;
  LastStrToIntResult:=C;
  StrToInt:=L;
end;

function GetStr(P: PString): string;
begin
  if P=nil then GetStr:='' else GetStr:=P^;
end;


function EatIO: integer;
begin
  EatIO:=IOResult;
end;



function TUnsortedStringCollection.At(Index: Integer): PString;
begin
  At:=inherited At(Index);
end;

procedure TUnsortedStringCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeStr(Item);
end;

END.
{
  $Log$
  Revision 1.1  1999-03-01 15:51:43  peter
    + Log

}
