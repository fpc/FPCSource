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

uses Objects;

const
{$ifdef Unix}
  dirsep = '/';
  listsep = [';',':'];
  exeext = '';
  pasext = '.pas';
  ppext  = '.pp';
{$else}
  dirsep = '\';
  listsep = [';'];
  exeext = '.exe';
  pasext = '.pas';
  ppext  = '.pp';
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
  dir,s : string;
  i : longint;
begin
  LocateExeFile:=False;
  if ExistsFile(FileName) then
    begin
      LocateExeFile:=true;
      Exit;
    end;

  S:=GetEnv('PATH');
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
    end;
  until I=0;
end;


END.
{
  $Log$
  Revision 1.1  2001-08-04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1.2.7  2001/03/06 21:57:29  pierre
   * some more generic code

  Revision 1.1.2.6  2000/11/27 11:57:29  pierre
   * some duplicates with wutils commented out

  Revision 1.1.2.5  2000/11/14 09:23:56  marco
   * Second batch

  Revision 1.1.2.4  2000/11/13 16:59:09  pierre
   * some function in double removed from fputils unit

  Revision 1.1.2.3  2000/11/03 15:45:57  pierre
   * fix LTrim for AnsiStrings

  Revision 1.1.2.2  2000/08/15 03:40:53  peter
   [*] no more fatal exits when the IDE can't find the error file (containing
       the redirected assembler/linker output) after compilation
   [*] hidden windows are now added always at the end of the Window List
   [*] TINIFile parsed entries encapsulated in string delimiters incorrectly
   [*] selection was incorrectly adjusted when typing in overwrite mode
   [*] the line wasn't expanded when it's end was reached in overw. mode
   [*] the IDE now tries to locate source files also in the user specified
       unit dirs (for ex. as a response to 'Open at cursor' (Ctrl+Enter) )
   [*] 'Open at cursor' is now aware of the extension (if specified)

  Revision 1.1.2.1  2000/07/20 11:02:15  michael
  + Fixes from gabor. See fixes.txt

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.16  2000/06/22 09:07:13  pierre
   * Gabor changes: see fixes.txt

  Revision 1.15  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.14  2000/01/03 11:38:34  michael
  Changes from Gabor

  Revision 1.13  1999/04/15 08:58:07  peter
    * syntax highlight fixes
    * browser updates

  Revision 1.12  1999/04/07 21:55:55  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.11  1999/03/19 16:04:31  peter
    * new compiler dialog

  Revision 1.10  1999/03/08 14:58:14  peter
    + prompt with dialogs for tools

  Revision 1.9  1999/03/01 15:42:06  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.8  1999/02/22 02:15:20  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.7  1999/02/16 17:13:55  pierre
   + findclose added for FPC

  Revision 1.6  1999/02/05 12:12:01  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.5  1999/02/02 16:41:43  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.4  1999/01/21 11:54:25  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.3  1999/01/12 14:29:40  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.2  1998/12/28 15:47:53  peter
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
