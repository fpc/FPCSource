{****************************************************************************

    $Id$

                         Free Pascal Runtime-Library
                              DOS unit for OS/2
                   Copyright (c) 1997,1999-2000 by Daniel Mantione,
                   member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit dos;

{$ASMMODE ATT}

{***************************************************************************}

interface

{***************************************************************************}

{$PACKRECORDS 1}

uses    Strings, DosCalls;

const
   FileNameLen = 255;

Type
   {Search record which is used by findfirst and findnext:}
   SearchRec = record
            case boolean of
             false: (Handle: THandle;     {Used in os_OS2 mode}
                     FStat: PFileFindBuf3;
                     Fill: array [1..21 - SizeOf (THandle) - SizeOf (pointer)]
                                                                       of byte;
                     Attr: byte;
                     Time: longint;
                     Size: longint;
                     Name: string);      {Filenames can be long in OS/2!}
             true:  (Fill2: array [1..21] of byte;
                     Attr2: byte;
                     Time2: longint;
                     Size2: longint;
                     Name2: string);       {Filenames can be long in OS/2!}
        end;

        {Data structure for the registers needed by msdos and intr:}
       registers=packed record
            case i:integer of
                0:(ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,
                   f8,flags,fs,gs:word);
                1:(al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh:byte);
                2:(eax,ebx,ecx,edx,ebp,esi,edi:longint);
            end;

        {Flags for the exec procedure:

        }

{$ifdef HASTHREADVAR}
threadvar
{$else HASTHREADVAR}
var
{$endif HASTHREADVAR}
(* For compatibility with VP/2, used for runflags in Exec procedure. *)
    ExecFlags: cardinal;

{$i dosh.inc}

{OS/2 specific functions}

function GetEnvPChar (EnvVar: string): PChar;

function DosErrorModuleName: string;
(* In case of an error in Dos.Exec returns the name of the module *)
(* causing the problem - e.g. name of a missing or corrupted DLL. *)



implementation


{$ifdef HASTHREADVAR}
threadvar
{$else HASTHREADVAR}
var
{$endif HASTHREADVAR}
  LastDosExitCode: longint;
  LastDosErrorModuleName: string;


const   FindResvdMask = $00003737; {Allowed bits in attribute
                                    specification for DosFindFirst call.}



function fsearch(path:pathstr;dirlist:string):pathstr;
Var
  A: array [0..255] of char;
  D, P: AnsiString;
begin
  P:=Path;
  D:=DirList;
  DosError:=DosSearchPath(0, PChar(D), PChar(P), @A, 255);
  fsearch := StrPas (@A);
end;


procedure getftime(var f;var time:longint);
var
  FStat: TFileStatus3;
begin
  DosError := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
  if DosError=0 then
  begin
    Time := FStat.TimeLastWrite + longint (FStat.DateLastWrite) shl 16;
    if Time = 0 then
      Time := FStat.TimeCreation + longint (FStat.DateCreation) shl 16;
  end else
    Time:=0;
end;


procedure SetFTime (var F; Time: longint);
var FStat: TFileStatus3;
    RC: cardinal;
begin
  RC := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
  if RC = 0 then
  begin
    FStat.DateLastAccess := Hi (Time);
    FStat.DateLastWrite := Hi (Time);
    FStat.TimeLastAccess := Lo (Time);
    FStat.TimeLastWrite := Lo (Time);
    RC := DosSetFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
  end;
  DosError := integer (RC);
end;


procedure Exec (const Path: PathStr; const ComLine: ComStr);
{Execute a program.}
var Args: PByteArray;
    ArgSize: word;
    Res: TResultCodes;
    ObjName: string;
const
    MaxArgsSize = 2048; (* Amount of memory reserved for arguments in bytes. *)
begin
{  LastDosExitCode := Exec (Path, ExecRunFlags (ExecFlags), efDefault, ComLine);}
    GetMem (Args, MaxArgsSize);
    ArgSize := 0;
    Move (Path [1], Args^ [ArgSize], Length (Path));
    Inc (ArgSize, Length (Path));
    Args^ [ArgSize] := 0;
    Inc (ArgSize);
    {Now do the real arguments.}
    Move (ComLine [1], Args^ [ArgSize], Length (ComLine));
    Inc (ArgSize, Length (ComLine));
    Args^ [ArgSize] := 0;
    Inc (ArgSize);
    Args^ [ArgSize] := 0;
    DosError := DosExecPgm (ObjName, cardinal (ExecFlags), Args, nil, Res, Path);
    if DosError = 0 then
      begin
        LastDosExitCode := Res.ExitCode;
        LastDosErrorModuleName := '';
      end
    else
      begin
        LastDosErrorModuleName := ObjName;
        LastDosExitCode := 0; (* Needed for TP/BP compatibility *)
      end;
    FreeMem (Args, MaxArgsSize);
end;


function DosExitCode: word;
begin
  DosExitCode := LastDosExitCode and $FFFF;
end;


function DosErrorModuleName: string;
begin
  DosErrorModuleName := LastDosErrorModuleName;
end;


function dosversion:word;
{Returns OS/2 version}
var
  Minor, Major: Cardinal;
begin
  DosQuerySysInfo(svMajorVersion, svMajorVersion, Major, 4);
  DosQuerySysInfo(svMinorVersion, svMinorVersion, Minor, 4);
  DosVersion:=Major or Minor shl 8;
end;


procedure GetDate (var Year, Month, MDay, WDay: word);
Var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Year:=dt.year;
  Month:=dt.month;
  MDay:=dt.Day;
  WDay:=dt.Weekday;
end;


procedure SetDate (Year, Month, Day: word);
var
  DT: TDateTime;
begin
  DosGetDateTime (DT);
  DT.Year := Year;
  DT.Month := byte (Month);
  DT.Day := byte (Day);
  DosSetDateTime (DT);
end;


procedure GetTime (var Hour, Minute, Second, Sec100: word);
var
  dt: TDateTime;
begin
  DosGetDateTime(dt);
  Hour:=dt.Hour;
  Minute:=dt.Minute;
  Second:=dt.Second;
  Sec100:=dt.Hundredths;
end;


procedure SetTime (Hour, Minute, Second, Sec100: word);
var
  DT: TDateTime;
begin
  DosGetDateTime (DT);
  DT.Hour := byte (Hour);
  DT.Minute := byte (Minute);
  DT.Second := byte (Second);
  DT.Sec100 := byte (Sec100);
  DosSetDateTime (DT);
end;


procedure getcbreak(var breakvalue:boolean);
begin
  breakvalue := True;
end;


procedure setcbreak(breakvalue:boolean);
begin
end;


procedure getverify(var verify:boolean);
begin
  verify := true;
end;


procedure setverify(verify:boolean);
begin
end;


function DiskFree (Drive: byte): int64;
var FI: TFSinfo;
    RC: cardinal;
begin
  {In OS/2, we use the filesystem information.}
  RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskFree := int64 (FI.Free_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskFree := -1;
end;


function DiskSize (Drive: byte): int64;
var FI: TFSinfo;
    RC: cardinal;
begin
  RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskSize := int64 (FI.Total_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskSize := -1;
end;


procedure SearchRec2DosSearchRec (var F: SearchRec);
begin
end;


procedure DosSearchRec2SearchRec (var F: SearchRec);
type
  TRec = record
    T, D: word;
  end;
begin
 with F do
    begin
        Name := FStat^.Name;
        Size := FStat^.FileSize;
        Attr := byte(FStat^.AttrFile and $FF);
        TRec (Time).T := FStat^.TimeLastWrite;
        TRec (Time).D := FStat^.DateLastWrite;
    end;
end;


procedure FindFirst (const Path: PathStr; Attr: word; var F: SearchRec);


var Count: cardinal;

begin
  {No error.}
  DosError := 0;
  New (F.FStat);
  F.Handle := THandle ($FFFFFFFF);
  Count := 1;
  DosError := integer (DosFindFirst (Path, F.Handle,
                     Attr and FindResvdMask, F.FStat, SizeOf (F.FStat^),
                                                           Count, ilStandard));
  if (DosError = 0) and (Count = 0) then DosError := 18;
  DosSearchRec2SearchRec (F);
end;

procedure FindNext (var F: SearchRec);
var
  Count: cardinal;
begin
    {No error}
    DosError := 0;
    SearchRec2DosSearchRec (F);
    Count := 1;
    DosError := integer (DosFindNext (F.Handle, F.FStat, SizeOf (F.FStat^),
                                                                       Count));
    if (DosError = 0) and (Count = 0) then DosError := 18;
    DosSearchRec2SearchRec (F);
end;

procedure FindClose (var F: SearchRec);
begin
  if F.Handle <> THandle ($FFFFFFFF) then DosError := DosFindClose (F.Handle);
  Dispose (F.FStat);
end;

procedure swapvectors;
{For TP compatibility, this exists.}
begin
end;

function envcount:longint;
begin
  envcount:=envc;
end;

function envstr (index : longint) : string;

var hp:Pchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=EnvP[index-1];
    envstr:=strpas(hp);
end;

function GetEnvPChar (EnvVar: string): PChar;
(* The assembler version is more than three times as fast as Pascal. *)
var
 P: PChar;
begin
 EnvVar := UpCase (EnvVar);
{$ASMMODE INTEL}
 asm
  cld
  mov edi, Environment
  lea esi, EnvVar
  xor eax, eax
  lodsb
@NewVar:
  cmp byte ptr [edi], 0
  jz @Stop
  push eax        { eax contains length of searched variable name }
  push esi        { esi points to the beginning of the variable name }
  mov ecx, -1     { our character ('=' - see below) _must_ be found }
  mov edx, edi    { pointer to beginning of variable name saved in edx }
  mov al, '='     { searching until '=' (end of variable name) }
  repne
  scasb           { scan until '=' not found }
  neg ecx         { what was the name length? }
  dec ecx         { corrected }
  dec ecx         { exclude the '=' character }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  push eax        { and save both of them again for later use }
  push esi
  cmp ecx, eax    { compare length of searched variable name with name }
  jnz @NotEqual   { ... of currently found variable, jump if different }
  xchg edx, edi   { pointer to current variable name restored in edi }
  repe
  cmpsb           { compare till the end of variable name }
  xchg edx, edi   { pointer to beginning of variable contents in edi }
  jz @Equal       { finish if they're equal }
@NotEqual:
  xor eax, eax    { look for 00h }
  mov ecx, -1     { it _must_ be found }
  repne
  scasb           { scan until found }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  jmp @NewVar     { ... or continue with new variable otherwise }
@Stop:
  xor eax, eax
  mov P, eax      { Not found - return nil }
  jmp @End
@Equal:
  pop esi         { restore the stack position }
  pop eax
  mov P, edi      { place pointer to variable contents in P }
@End:
 end ['eax','ecx','edx','esi','edi'];
 GetEnvPChar := P;
end;
{$ASMMODE ATT}

Function GetEnv(envvar: string): string;
(* The assembler version is more than three times as fast as Pascal. *)
begin
 GetEnv := StrPas (GetEnvPChar (EnvVar));
end;

procedure fsplit(path:pathstr;var dir:dirstr;var name:namestr;
                 var ext:extstr);

var p1,i : longint;
    dotpos : integer;

begin
    { allow slash as backslash }
    for i:=1 to length(path) do
      if path[i]='/' then path[i]:='\';
    {Get drive name}
    p1:=pos(':',path);
    if p1>0 then
        begin
            dir:=path[1]+':';
            delete(path,1,p1);
        end
    else
        dir:='';
    { split the path and the name, there are no more path informtions }
    { if path contains no backslashes                                 }
    while true do
        begin
            p1:=pos('\',path);
            if p1=0 then
                break;
            dir:=dir+copy(path,1,p1);
              delete(path,1,p1);
        end;
   { try to find out a extension }
   Ext:='';
   i:=Length(Path);
   DotPos:=256;
   While (i>0) Do
     Begin
       If (Path[i]='.') Then
         begin
           DotPos:=i;
           break;
         end;
       Dec(i);
     end;
   Ext:=Copy(Path,DotPos,255);
   Name:=Copy(Path,1,DotPos - 1);
end;

(*
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
*)

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_DRIVES}
{$UNDEF FPC_FEXPAND_UNC}

procedure packtime(var t:datetime;var p:longint);

var zs:longint;

begin
    p:=-1980;
    p:=p+t.year and 127;
    p:=p shl 4;
    p:=p+t.month;
    p:=p shl 5;
    p:=p+t.day;
    p:=p shl 16;
    zs:=t.hour;
    zs:=zs shl 6;
    zs:=zs+t.min;
    zs:=zs shl 5;
    zs:=zs+t.sec div 2;
    p:=p+(zs and $ffff);
end;

procedure unpacktime (p:longint;var t:datetime);

begin
    t.sec:=(p and 31) * 2;
    p:=p shr 5;
    t.min:=p and 63;
    p:=p shr 6;
    t.hour:=p and 31;
    p:=p shr 5;
    t.day:=p and 31;
    p:=p shr 5;
    t.month:=p and 15;
    p:=p shr 4;
    t.year:=p+1980;
end;

procedure GetFAttr (var F; var Attr: word);
var
  PathInfo: TFileStatus3;
  RC: cardinal;
begin
  Attr := 0;
  RC := DosQueryPathInfo (FileRec (F).Name, ilStandard,
                                                 @PathInfo, SizeOf (PathInfo));
  DosError := integer (RC);
  if RC = 0 then
    Attr := PathInfo.AttrFile;
end;

procedure SetFAttr (var F; Attr: word);
var
  PathInfo: TFileStatus3;
  RC: cardinal;
begin
  RC := DosQueryPathInfo (FileRec (F).Name, ilStandard,
                                                 @PathInfo, SizeOf (PathInfo));
  if RC = 0 then
  begin
    PathInfo.AttrFile := Attr;
    RC := DosSetPathInfo (FileRec (F).Name, ilStandard, @PathInfo,
                                               SizeOf (PathInfo), doWriteThru);
  end;
  DosError := integer (RC);
end;



{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

procedure Keep (ExitCode: word);
begin
end;

procedure GetIntVec (IntNo: byte; var Vector: pointer);
begin
end;

procedure SetIntVec (IntNo: byte; Vector: pointer);
begin
end;

procedure Intr (IntNo: byte; var Regs: Registers);
begin
end;

procedure MsDos (var Regs: Registers);
begin
end;


function  GetShortName(var p : String) : boolean;
begin
  GetShortName:=true;
{$WARNING EA .shortname support (see FAT32 driver) should be probably added here!}
end;

function  GetLongName(var p : String) : boolean;
begin
  GetLongName:=true;
{$WARNING EA .longname support should be probably added here!}
end;



begin
 LastDosExitCode := 0;
 LastDosErrorModuleName := '';
 ExecFlags := 0;
end.

{
  $Log$
  Revision 1.41  2004-05-23 21:47:34  hajny
    * final part of longint2cardinal fixes for doscalls

  Revision 1.40  2004/03/21 20:22:20  hajny
    * Exec cleanup

  Revision 1.39  2004/02/22 15:01:49  hajny
    * lots of fixes (regcall, THandle, string operations in sysutils, longint2cardinal according to OS/2 docs, dosh.inc, ...)

  Revision 1.38  2004/02/17 17:37:26  daniel
    * Enable threadvars again

  Revision 1.37  2004/02/16 22:16:59  hajny
    * LastDosExitCode changed back from threadvar temporarily

  Revision 1.36  2004/02/15 21:34:06  hajny
    * overloaded ExecuteProcess added, EnvStr param changed to longint

  Revision 1.35  2004/02/15 08:02:44  yuri
  * fixes for dosh.inc
  * Executeprocess iverloaded function
  * updated todo

  Revision 1.34  2004/02/09 12:03:16  michael
  + Switched to single interface in dosh.inc

  Revision 1.33  2003/11/05 09:13:59  yuri
  * exec fix
  * unused units removed

  Revision 1.32  2003/11/02 09:45:32  hajny
  SetFTime fix

  Revision 1.31  2003/11/01 18:35:12  hajny
    * GetFTime correction for case of no previous write access

  Revision 1.30  2003/10/25 23:55:22  hajny
    * Exec fix

  Revision 1.29  2003/10/25 22:45:37  hajny
    * file handling related fixes

  Revision 1.28  2003/10/05 22:06:43  hajny
    * result buffers must be allocated

  Revision 1.27  2003/10/03 21:46:41  peter
    * stdcall fixes

  Revision 1.26  2003/09/24 08:59:16  yuri
  * Prepared for native target (emx code replaced)

  Revision 1.25  2003/02/20 17:37:00  hajny
    * correction for previous mistyping

  Revision 1.24  2003/02/20 17:09:49  hajny
    * fixes for OS/2 v2.1 incompatibility

  Revision 1.23  2003/01/04 15:43:50  hajny
    + GetEnvPChar added

  Revision 1.22  2002/12/07 19:46:56  hajny
    * mistyping fixed

  Revision 1.21  2002/12/07 19:17:13  hajny
    * GetEnv correction, better PM support, ...

  Revision 1.20  2002/11/18 19:51:00  hajny
    * another bunch of type corrections

  Revision 1.19  2002/09/07 16:01:24  peter
    * old logs removed and tabs fixed

  Revision 1.18  2002/07/11 16:00:05  hajny
    * FindFirst fix (invalid attribute bits masked out)

  Revision 1.17  2002/07/07 18:00:48  hajny
    * DosGetInfoBlock modification to allow overloaded version (in DosCalls)

  Revision 1.16  2002/03/03 11:19:20  hajny
    * GetEnv rewritten to assembly - 3x faster now

}
