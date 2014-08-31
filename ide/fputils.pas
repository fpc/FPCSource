{
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

uses
  Sysutils,
  Objects;

const
  dirsep = System.DirectorySeparator;

{$ifdef Unix}
  listsep = [';',':'];
  exeext = '';
  pasext = '.pas';
  ppext  = '.pp';
{$else}
  {$ifdef HASAMIGA}
    listsep = [';'];
    exeext = '';
    pasext = '.pas';
    ppext  = '.pp';
  {$else HASAMIGA}
  listsep = [';'];
  exeext = '.exe';
  pasext = '.pas';
  ppext  = '.pp';
  {$endif HASAMIGA}
{$endif}


function SmartPath(Path: string): string;
Function FixPath(s:string;allowdot:boolean):string;
function FixFileName(const s:string):string;
function MakeExeName(const fn:string):string;
function Center(const S: string; Len: byte): string;
function FitStr(const S: string; Len: byte): string;
function KillTilde(S: string): string;
function LowercaseStr(const S: string): string;
{function DirOf(const S: string): string;
function ExtOf(const S: string): string;
function NameOf(const S: string): string;
function NameAndExtOf(const S: string): string;}
function StrToExtended(S: string): Extended;
function Power(const A,B: double): double;
function MatchesMask(What, Mask: string): boolean;
function MatchesMaskList(What, MaskList: string): boolean;
function MatchesFileList(What, FileList: string): boolean;
function EatIO: integer;
function RenameFile(const OldFileName,NewFileName: string): boolean;
function LocateFile(FileList: string): string;
function LocatePasFile(const FileName:string):string;
function LocateExeFile(var FileName:string): boolean;
function EraseFile(FileName: string): boolean;
function GetStr(const P: PString): string;
procedure ReplaceStr(var S: string; const What,NewS: string);
procedure ReplaceStrI(var S: string; What: string; const NewS: string);

const ListSeparator      : char = ';';

implementation

uses Dos,
     WUtils,
     FPVars,FPSwitch;

function IntToStr(L: longint): string;
var S: string;
begin
  Str(L,S);
  IntToStr:=S;
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
  GetDir(0,S);
  if copy(S,length(S),1)<>DirSep then S:=S+DirSep;
{$ifdef FSCaseInsensitive}
  if (LowerCaseStr(copy(Path,1,length(S)))=LowerCaseStr(S)) {and (Pos('\',copy(Path,length(S)+1,High(S)))=0)} then
{$else}
  if (copy(Path,1,length(S))=S) {and (Pos('\',copy(Path,length(S)+1,High(S)))=0)} then
{$endif}
     system.Delete(Path,1,length(S));
  SmartPath:=Path;
end;


Function FixPath(s:string;allowdot:boolean):string;
var
  i : longint;
begin
  for i:=1 to length(s) do
   if s[i] in ['/','\'] then
    s[i]:=DirSep;
  if (length(s)>0) and (s[length(s)]<>DirSep) and
     (s[length(s)]<>':') then
   s:=s+DirSep;
  if (not allowdot) and (s='.'+DirSep) then
   s:='';
  FixPath:=s;
end;


function FixFileName(const s:string):string;
var
  i      : longint;
{$ifdef Unix}
  NoPath : boolean;
{$endif}
begin
  {$ifdef Unix}NoPath:=true;{$endif}
  for i:=length(s) downto 1 do
   begin
     case s[i] of
 {$ifdef Unix}
  '/','\' : begin
              FixFileName[i]:='/';
              NoPath:=false; {Skip lowercasing path: 'X11'<>'x11' }
            end;
 'A'..'Z' : if NoPath then
             FixFileName[i]:=char(byte(s[i])+ord('a')-ord('A'))
            else
             FixFileName[i]:=s[i];
 {$else}
      '/' : FixFileName[i]:='\';
 'A'..'Z' : FixFileName[i]:=char(byte(s[i])+32);
 {$endif}
     else
      FixFileName[i]:=s[i];
     end;
   end;
  FixFileName[0]:=s[0];
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


function Center(const S: string; Len: byte): string;
begin
  Center:=LExpand(S+CharStr(' ',Max(0,(Len-length(S)) div 2)),Len);
end;

function FitStr(const S: string; Len: byte): string;
begin
  FitStr:=RExpand(copy(S,1,Len),Len);
end;


function KillTilde(S: string): string;
var P: longint;
begin
  repeat
    P:=Pos('~',S);
    if P>0 then
      Delete(S,P,1);
  until P=0;
  KillTilde:=S;
end;

function LowerCaseStr(const S: string): string;
var
  I: Longint;
begin
  for I:=1 to length(S) do
    if S[I] in ['A'..'Z'] then
      LowerCaseStr[I]:=chr(ord(S[I])+32)
    else
      LowerCaseStr[I]:=S[I];
  LowercaseStr[0]:=S[0];
end;

{function DirOf(const S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  if (D<>'') and (D[Length(D)]<>DirSep) then
   DirOf:=D+DirSep
  else
   DirOf:=D;
end;


function ExtOf(const S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  ExtOf:=E;
end;


function NameOf(const S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  NameOf:=N;
end;

function NameAndExtOf(const S: string): string;
var D: DirStr; E: ExtStr; N: NameStr;
begin
  FSplit(S,D,N,E);
  NameAndExtOf:=N+E;
end; }

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

function MatchesMask(What, Mask: string): boolean;

  function upper(const s : string) : string;
  var
    i  : Sw_integer;
  begin
     for i:=1 to length(s) do
      if s[i] in ['a'..'z'] then
       upper[i]:=char(byte(s[i])-32)
      else
       upper[i]:=s[i];
     upper[0]:=s[0];
  end;

  Function CmpStr(const hstr1,hstr2:string):boolean;
  var
    found : boolean;
    i1,i2 : Sw_integer;
  begin
    i1:=0;
    i2:=0;
    found:=true;
    while found and (i1<length(hstr1)) and (i2<=length(hstr2)) do
     begin
       if found then
        inc(i2);
       inc(i1);
       case hstr1[i1] of
         '?' :
           found:=true;
         '*' :
           begin
             found:=true;
             if (i1=length(hstr1)) then
              i2:=length(hstr2)
             else
              if (i1<length(hstr1)) and (hstr1[i1+1]<>hstr2[i2]) then
               begin
                 if i2<length(hstr2) then
                  dec(i1)
               end
             else
              if i2>1 then
               dec(i2);
           end;
         else
           found:=(hstr1[i1]=hstr2[i2]) or (hstr2[i2]='?');
       end;
     end;
    if found then
      found:=(i1>=length(hstr1)) and (i2>=length(hstr2));
    CmpStr:=found;
  end;

var
  D1,D2 : DirStr;
  N1,N2 : NameStr;
  E1,E2 : Extstr;
begin
{$ifdef Unix}
  FSplit(What,D1,N1,E1);
  FSplit(Mask,D2,N2,E2);
{$else}
  FSplit(Upper(What),D1,N1,E1);
  FSplit(Upper(Mask),D2,N2,E2);
{$endif}
  MatchesMask:=CmpStr(N2,N1) and CmpStr(E2,E1);
end;

function MatchesMaskList(What, MaskList: string): boolean;
var P: integer;
    Match: boolean;
begin
  Match:=false;
  if What<>'' then
  repeat
    P:=Pos(ListSeparator, MaskList);
    if P=0 then
      P:=length(MaskList)+1;
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

function RenameFile(const OldFileName,NewFileName: string): boolean;
var f: file;
begin
  Assign(f,OldFileName);
  Rename(f,NewFileName);
  RenameFile:=(EatIO=0);
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
  if OK=false then OK:=CheckFile(IDEDir,FileName);
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

function LocatePasFile(const FileName:string):string;
var
  s : string;
begin
  LocatePasFile:=FileName;
  if ExistsFile(FileName) or (ExtOf(FileName)<>'') then
   exit;
  S:=FileName+PPExt;
  if ExistsFile(S) then
   begin
     LocatePasFile:=S;
     exit;
   end;
  S:=FileName+PasExt;
  if ExistsFile(S) then
   begin
     LocatePasFile:=S;
     exit;
   end;
end;

function LocateExeFile(var FileName:string): boolean;
var
  dir : string;
  s : ansistring;
  i : longint;
begin
  LocateExeFile:=False;
  if ExistsFile(FileName) then
    begin
      LocateExeFile:=true;
      Exit;
    end;

  S:=sysutils.GetEnvironmentVariable('PATH');
  While Length(S)>0 do
    begin
      i:=1;
      While (i<=Length(S)) and not (S[i] in ListSep) do
        Inc(i);
      Dir:=CompleteDir(Copy(S,1,i-1));
      if i<Length(S) then
        Delete(S,1,i)
      else
        S:='';
      if ExistsFile(Dir+FileName) then
        Begin
           FileName:=Dir+FileName;
           LocateExeFile:=true;
           Exit;
        End;
   end;
end;

function GetStr(const P: PString): string;
begin
  if P=nil then GetStr:='' else GetStr:=P^;
end;

function EraseFile(FileName: string): boolean;
var f: file;
begin
  if FileName='' then Exit;
  {$I-}
  Assign(f,FileName);
  Erase(f);
  {$I+}
  EraseFile:=(EatIO=0);
end;

procedure ReplaceStr(var S: string; const What,NewS: string);
var I : Sw_integer;
begin
  repeat
    I:=Pos(What,S);
    if I>0 then
    begin
      Delete(S,I,length(What));
      Insert(NewS,S,I);
    end;
  until I=0;
end;

procedure ReplaceStrI(var S: string; What: string; const NewS: string);
var I : integer;
    UpcaseS: string;
begin
  UpcaseS:=UpcaseStr(S); What:=UpcaseStr(What);
  repeat
    I:=Pos(What,UpcaseS);
    if I>0 then
    begin
      Delete(S,I,length(What));
      Insert(NewS,S,I);
      Delete(UpcaseS,I,length(What));
      Insert(NewS,UpcaseS,I);
    end;
  until I=0;
end;


END.
