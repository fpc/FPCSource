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
  exeext = '';
{$else}
  dirsep = '\';
  exeext = '.exe';
{$endif}

function IntToStr(L: longint): string;
function IntToStrZ(L: longint; MinLen: byte): string;
function IntToStrL(L: longint; MinLen: byte): string;
function StrToInt(S: string): longint;
function IntToHex(L: longint): string;
function IntToHexL(L: longint; MinLen: byte): string;
function HexToInt(S: string): longint;
function CharStr(C: char; Count: byte): string;
function SmartPath(Path: string): string;
function MakeExeName(const fn:string):string;
function LExpand(S: string; MinLen: byte): string;
function RExpand(S: string; MinLen: byte): string;
function LTrim(S: string): string;
function RTrim(S: string): string;
function Trim(S: string): string;
function KillTilde(S: string): string;
function UpcaseStr(S: string): string;
function LowerCaseStr(S: string): string;
function Max(A,B: longint): longint;
function Min(A,B: longint): longint;
function DirOf(S: string): string;
function NameOf(S: string): string;
function NameAndExtOf(S: string): string;
function StrToExtended(S: string): Extended;
function Power(const A,B: double): double;
function GetCurDir: string;
function MatchesMask(What, Mask: string): boolean;
function MatchesMaskList(What, MaskList: string): boolean;
function MatchesFileList(What, FileList: string): boolean;
function EatIO: integer;
function ExistsFile(FileName: string): boolean;
function CompleteDir(Path: string): string;
function LocateFile(FileList: string): string;

const LastStrToIntResult : integer = 0;
      LastHexToIntResult : integer = 0;
      ListSeparator      : char = ';';

implementation

uses Dos,
     FPVars;

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
  LastStrToIntResult:=C;
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


function MakeExeName(const fn:string):string;
var
  d : DirStr;
  n : NameStr;
  e : ExtStr;
begin
  FSplit(fn,d,n,e);
  MakeExeName:=d+n+ExeExt;
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

function NameAndExtOf(S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  NameAndExtOf:=N+E;
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

function GetCurDir: string;
var S: string;
begin
  GetDir(0,S);
  if copy(S,length(S),1)<>DirSep then S:=S+DirSep;
  GetCurDir:=S;
end;

function IntToHex(L: longint): string;
const HexNums : string[16] = '0123456789ABCDEF';
var S: string;
    R: real;
function DivF(Mit,Mivel: real): longint;
begin
  DivF:=trunc(Mit/Mivel);
end;
function ModF(Mit,Mivel: real): longint;
begin
  ModF:=trunc(Mit-DivF(Mit,Mivel)*Mivel);
end;
begin
  S:='';
  R:=L; if R<0 then begin R:=R+2147483647+2147483647+2; end;
  repeat
    S:=HexNums[ModF(R,16)+1]+S;
    R:=DivF(R,16);
  until R=0;
  IntToHex:=S;
end;

function HexToInt(S: string): longint;
var L,I: longint;
    C: char;
const HexNums: string[16] = '0123456789ABCDEF';
begin
  S:=Trim(S); L:=0; I:=1; LastHexToIntResult:=0;
  while (I<=length(S)) and (LastHexToIntResult=0) do
  begin
    C:=Upcase(S[I]);
    if C in['0'..'9','A'..'F'] then
    begin
      L:=L*16+(Pos(C,HexNums)-1);
    end else LastHexToIntResult:=I;
    Inc(I);
  end;
  HexToInt:=L;
end;

function IntToHexL(L: longint; MinLen: byte): string;
var S: string;
begin
  S:=IntToHex(L);
  while length(S)<MinLen do S:='0'+S;
  IntToHexL:=S;
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

function MatchesMask(What, Mask: string): boolean;
var P: integer;
    Match: boolean;
begin
  P:=Pos('*',Mask);
  if P>0 then
    begin
      Mask:=copy(Mask,1,P-1);
      What:=copy(What,1,P-1);
    end;
  Match:=length(Mask)=length(What); P:=1;
  if Match and (Mask<>'') then
  repeat
    Match:=Match and ((Mask[P]='?') or (Upcase(Mask[P])=Upcase(What[P])));
    Inc(P);
  until (Match=false) or (P>length(Mask));
  MatchesMask:=Match;
end;

function MatchesMaskList(What, MaskList: string): boolean;
var P: integer;
    Match: boolean;
begin
  Match:=false;
  if What<>'' then
  repeat
    P:=Pos(ListSeparator, MaskList);
    if P=0 then P:=length(MaskList)+1;
    Match:=MatchesMask(What,copy(MaskList,1,P-1));
    Delete(MaskList,1,P);
  until Match or (MaskList='');
  MatchesMaskList:=Match;
end;

function MatchesFileList(What, FileList: string): boolean;
var P: integer;
    Match: boolean;
    WD,FD : record D: DirStr; N: NameStr; E: ExtStr; end;
    F: string;
begin
  Match:=false;
  FSplit(What,WD.D,WD.N,WD.E);
  if What<>'' then
  repeat
    P:=Pos(ListSeparator, FileList);
    if P=0 then P:=length(FileList)+1;
    F:=copy(FileList,1,P-1);
    FSplit(F,FD.D,FD.N,FD.E);
    Match:=MatchesMask(WD.D+WD.N,FD.D+FD.N) and
           MatchesMask(WD.E,FD.E);
    Delete(FileList,1,P);
  until Match or (FileList='');
  MatchesFileList:=Match;
end;

function EatIO: integer;
begin
  EatIO:=IOResult;
end;

function ExistsFile(FileName: string): boolean;
var f: file;
    Exists: boolean;
begin
  {$I-}
  Assign(f,FileName);
  Reset(f,1);
  Exists:=EatIO=0;
  Close(f);
  EatIO;
  {$I+}
  ExistsFile:=Exists;
end;

function CompleteDir(Path: string): string;
begin
  if copy(Path,length(Path),1)<>DirSep then Path:=Path+DirSep;
  CompleteDir:=Path;
end;

function LocateFile(FileList: string): string;
var FilePath: string;
function CheckFile(Path,Name: string): boolean;
var OK: boolean;
begin
  Path:=CompleteDir(Path);
  Path:=Path+Name;
  OK:=ExistsFile(Path);
  if OK then FilePath:=Path;
  CheckFile:=OK;
end;
function LocateSingleFile(FileName: string): boolean;
var OK: boolean;
begin
  OK:=CheckFile(FExpand('.'),FileName);
  if OK=false then OK:=CheckFile(StartupDir,FileName);
  if OK=false then OK:=CheckFile(DirOf(FExpand(ParamStr(0))),FileName);
  LocateSingleFile:=OK;
end;
var P: integer;
begin
  FilePath:='';
  if FileList<>'' then
  repeat
    P:=Pos(ListSeparator,FileList); if P=0 then P:=length(FileList)+1;
    LocateSingleFile(copy(FileList,1,P-1));
    Delete(FileList,1,P);
  until (FilePath<>'') or (FileList='');
  LocateFile:=FilePath;
end;


END.
{
  $Log$
  Revision 1.2  1998-12-28 15:47:53  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.31 1998/12/27 11:25:37  gabor
    + MatchesMask(), MatchesMaskList() and MatchesFileList() added
    + NameAndExtOf() added
  Revision 1.3  1998/12/22 10:39:52  peter
    + options are now written/read
    + find and replace routines

}
