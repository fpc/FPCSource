{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Utilility routines used by the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPUtils;

interface

const
{$ifdef linux}
  dirsep = '/';
{$else}
  dirsep = '\';
{$endif}

function IntToStr(L: longint): string;
function IntToStrZ(L: longint; MinLen: byte): string;
function IntToStrL(L: longint; MinLen: byte): string;
function StrToInt(S: string): longint;
function CharStr(C: char; Count: byte): string;
function SmartPath(Path: string): string;
function LExpand(S: string; MinLen: byte): string;
function RExpand(S: string; MinLen: byte): string;
function KillTilde(S: string): string;
function UpcaseStr(S: string): string;
function LowerCaseStr(S: string): string;
function Max(A,B: longint): longint;
function Min(A,B: longint): longint;
function DirOf(S: string): string;
function NameOf(S: string): string;
function StrToExtended(S: string): Extended;
function Power(const A,B: double): double;

implementation

uses Dos;

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
  Val(S,L,C);
  if C<>0 then L:=-1;
  StrToInt:=L;
end;

function CharStr(C: char; Count: byte): string;
var S: string;
begin
  S[0]:=chr(Count);
  FillChar(S[1],Count,C);
  CharStr:=S;
end;

function IntToStrZ(L: longint; MinLen: byte): string;
var S: string;
begin
  S:=IntToStr(L);
  if length(S)<MinLen then S:=CharStr('0',MinLen-length(S))+S;
  IntToStrZ:=S;
end;

function IntToStrL(L: longint; MinLen: byte): string;
var S: string;
begin
  S:=IntToStr(L);
  if length(S)<MinLen then S:=CharStr(' ',MinLen-length(S))+S;
  IntToStrL:=S;
end;

function SmartPath(Path: string): string;
var S: string;
begin
  GetDir(0,S); if copy(S,length(S),1)<>DirSep then S:=S+DirSep;
  if (copy(Path,1,length(S))=S) {and (Pos('\',copy(Path,length(S)+1,255))=0)} then
     system.Delete(Path,1,length(S));
  SmartPath:=Path;
end;

function LExpand(S: string; MinLen: byte): string;
begin
  if length(S)<MinLen then S:=CharStr(' ',MinLen-length(S))+S;
  LExpand:=S;
end;

function RExpand(S: string; MinLen: byte): string;
begin
  if length(S)<MinLen then S:=S+CharStr(' ',MinLen-length(S));
  RExpand:=S;
end;

function KillTilde(S: string): string;
var P: byte;
begin
  repeat
    P:=Pos('~',S);
    if P>0 then Delete(S,P,1);
  until P=0;
  KillTilde:=S;
end;

function UpcaseStr(S: string): string;
var I: integer;
begin
  for I:=1 to length(S) do
      S[I]:=Upcase(S[I]);
  UpcaseStr:=S;
end;

function LowerCaseStr(S: string): string;
var I: byte;
begin
  for I:=1 to length(S) do
    if S[I] in ['A'..'Z'] then S[I]:=chr(ord(S[I])+32);
  LowerCaseStr:=S;
end;

function Max(A,B: longint): longint;
begin
  if A>B then Max:=A else Max:=B;
end;

function Min(A,B: longint): longint;
begin
  if A<B then Min:=A else Min:=B;
end;

function DirOf(S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  if copy(D,1,length(D))<>DirSep then D:=D+DirSep;
  DirOf:=D;
end;

function NameOf(S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  NameOf:=N;
end;

function StrToExtended(S: string): Extended;
var R : Extended;
    C : integer;
begin
  Val(S,R,C);
  StrToExtended:=R;
end;

function Power(const A,B: double): double;
begin
  if A=0 then Power:=0
         else Power:=exp(B*ln(A));
end;


END.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:52  peter
    + options are now written/read
    + find and replace routines

}
