{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2004-2005 by Olle Raab

    Sysutils unit for Mac OS.

    NOTE !!! THIS FILE IS UNDER CONSTRUCTION AND DOES NOT WORK CURRENLY.

    THUS IT IS NOT BUILT BY THE MAKEFILES

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysutils;
interface

{$MODE objfpc}
{ force ansistrings }
{$H+}

uses
  MacOSTP;

//{$DEFINE HAS_SLEEP}     TODO
//{$DEFINE HAS_OSERROR}   TODO
//{$DEFINE HAS_OSCONFIG}  TODO

type
//TODO Check pad and size
//TODO unify with Dos.SearchRec
  PMacOSFindData = ^TMacOSFindData;
  TMacOSFindData = record
                {MacOS specific params, private, do not use:}
                paramBlock: CInfoPBRec;
                searchFSSpec: FSSpec;
                searchAttr: Byte;  {attribute we are searching for}
                exactMatch: Boolean;
  end;

{ Include platform independent interface part }
{$i sysutilh.inc}

implementation

uses
  Dos, Sysconst; // For some included files.

{$DEFINE FPC_FEXPAND_VOLUMES}
{$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}
{$DEFINE FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$DEFINE FPC_FEXPAND_NO_DOTS_UPDIR}
{$DEFINE FPC_FEXPAND_NO_CURDIR}

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  (* TODO fix
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileOpen:=fdOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
  *)
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  (* TODO fix
  FileCreate:=fdOpen(FileName,Open_RdWr or Open_Creat or Open_Trunc);
  *)
end;


Function FileCreate (Const FileName : String;Mode : Longint) : Longint;

Var LinuxFlags : longint;

BEGIN
  (* TODO fix
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileCreate:=fdOpen(FileName,LinuxFlags or Open_Creat or Open_Trunc);
  *)
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  (* TODO fix
  FileRead:=fdRead (Handle,Buffer,Count);
  *)
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  (* TODO fix
  FileWrite:=fdWrite (Handle,Buffer,Count);
  *)
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  (* TODO fix
  FileSeek:=fdSeek (Handle,FOffset,Origin);
  *)
end;


Function FileSeek (Handle : Longint; FOffset: Int64; Origin : Longint) : Int64;

begin
  (* TODO fix
  {$warning need to add 64bit call }
  FileSeek:=fdSeek (Handle,FOffset,Origin);
  *)
end;


Procedure FileClose (Handle : Longint);

begin
  (* TODO fix
  fdclose(Handle);
  *)
end;

Function FileTruncate (Handle: THandle; Size: Int64) : boolean;

begin
  (* TODO fix
  FileTruncate:=fdtruncate(Handle,Size);
  *)
end;

Function FileAge (Const FileName : String): Longint;

  (*
Var Info : Stat;
    Y,M,D,hh,mm,ss : word;
  *)

begin
  (* TODO fix
  If not fstat (FileName,Info) then
    exit(-1)
  else
    begin
    EpochToLocal(info.mtime,y,m,d,hh,mm,ss);
    Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
    end;
  *)
end;


Function FileExists (Const FileName : String) : Boolean;

  (*
Var Info : Stat;
  *)

begin
  (* TODO fix
  FileExists:=fstat(filename,Info);
  *)
end;


Function DirectoryExists (Const Directory : String) : Boolean;

  (*
Var Info : Stat;
  *)

begin
  (* TODO fix
  DirectoryExists:=fstat(Directory,Info) and
                   ((info.mode and STAT_IFMT)=STAT_IFDIR);
  *)
end;

(*
Function LinuxToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

begin
  Result:=faArchive;
  If (Info.Mode and STAT_IFDIR)=STAT_IFDIR then
    Result:=Result or faDirectory;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.Mode and STAT_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If (Info.Mode and
      (STAT_IFSOCK or STAT_IFBLK or STAT_IFCHR or STAT_IFIFO))<>0 then
     Result:=Result or faSysFile;
end;

{
 GlobToSearch takes a glob entry, stats the file.
 The glob entry is removed.
 If FileAttributes match, the entry is reused
}

Type
  TGlobSearchRec = Record
    Path       : String;
    GlobHandle : PGlob;
  end;
  PGlobSearchRec = ^TGlobSearchRec;

Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var SInfo : Stat;
    p     : Pglob;
    GlobSearchRec : PGlobSearchrec;

begin
  GlobSearchRec:=PGlobSearchrec(Info.FindHandle);
  P:=GlobSearchRec^.GlobHandle;
  Result:=P<>Nil;
  If Result then
    begin
    GlobSearchRec^.GlobHandle:=P^.Next;
    Result:=Fstat(GlobSearchRec^.Path+StrPas(p^.name),SInfo);
    If Result then
      begin
      Info.Attr:=LinuxToWinAttr(p^.name,SInfo);
      Result:=(Info.ExcludeAttr and Info.Attr)=0;
      If Result Then
         With Info do
           begin
           Attr:=Info.Attr;
           If P^.Name<>Nil then
           Name:=strpas(p^.name);
           Time:=Sinfo.mtime;
           Size:=Sinfo.Size;
           end;
      end;
    P^.Next:=Nil;
    GlobFree(P);
    end;
end;
*)


procedure DoFind (var F: TSearchRec; firstTime: Boolean);

  var
    err: OSErr;
    s: Str255;

begin
  with Rslt, findData, paramBlock do
    begin
      ioVRefNum := searchFSSpec.vRefNum;
      if firstTime then
        ioFDirIndex := 0;

      while true do
        begin
          s := '';
          ioDirID := searchFSSpec.parID;
          ioFDirIndex := ioFDirIndex + 1;
          ioNamePtr := @s;

          err := PBGetCatInfoSync(@paramBlock);

          if err <> noErr then
            begin
              if err = fnfErr then
                DosError := 18
              else
                DosError := MacOSErr2RTEerr(err);
              break;
            end;

          attr := GetFileAttrFromPB(Rslt.paramBlock);
          if ((Attr and not(searchAttr)) = 0) then
            begin
              name := s;
              UpperString(s, true);

              if FNMatch(Rslt.searchFSSpec.name, s) then
                begin
                  size := GetFileSizeFromPB(paramBlock);
                  time := MacTimeToDosPackedTime(ioFlMdDat);
                  Result := 0;
                  break;
                end;
            end;
        end;
    end;
end;


Function FindFirst (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
  var
    s: Str255;

begin
  fillchar(Rslt, sizeof(Rslt), 0);

  if path = '' then
    begin
      Result := 3;
      Exit;
    end;

  {We always also search for readonly and archive, regardless of Attr.}
  Rslt.searchAttr := (Attr or (archive or readonly));

  Result := PathArgToFSSpec(path, Rslt.searchFSSpec);
  with Rslt do
    if (Result = 0) or (Result = 2) then
      begin
        SearchSpec := path;
        NamePos := Length(path) - Length(searchFSSpec.name);

        if (Pos('?', searchFSSpec.name) = 0) and (Pos('*', searchFSSpec.name) = 0) then  {No wildcards}
          begin  {If exact match, we don't have to scan the directory}
            exactMatch := true;
            Result := DoFindOne(searchFSSpec, paramBlock);
            if Result = 0 then
              begin
                Attr := GetFileAttrFromPB(paramBlock);
                if ((Attr and not(searchAttr)) = 0) then
                  begin
                    name := searchFSSpec.name;
                    size := GetFileSizeFromPB(paramBlock);
                    time := MacTimeToDosPackedTime(paramBlock.ioFlMdDat);
                  end
                else
                  Result := 18;
              end
            else if Result = 2 then
              Result := 18;
          end
        else
          begin
            exactMatch := false;

            s := searchFSSpec.name;
            UpperString(s, true);
            Rslt.searchFSSpec.name := s;

            DoFind(Rslt, true);
          end;
      end;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  if F.exactMatch then
    Result := 18
  else
    Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

  (*
Var
  GlobSearchRec : PGlobSearchRec;
  *)

begin
  (* TODO fix
  GlobSearchRec:=PGlobSearchRec(F.FindHandle);
  GlobFree (GlobSearchRec^.GlobHandle);
  Dispose(GlobSearchRec);
  *)
end;


Function FileGetDate (Handle : Longint) : Longint;

  (*
Var Info : Stat;
  *)

begin
  (* TODO fix
  If Not(FStat(Handle,Info)) then
    Result:=-1
  else
    Result:=Info.Mtime;
  *)
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // TODO fix
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

  (*
Var Info : Stat;
  *)

begin
  (* TODO fix
  If Not FStat (FileName,Info) then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(FileName),Info);
  *)
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  (* TODO fix
  Result:=UnLink (FileName);
  *)
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  (* TODO fix
  RenameFile:=Unix.FRename(OldNAme,NewName);
  *)
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}

{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the statfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',
    '/fd0/.',
    '/fd1/.',
    '/.'
    );
var
  Drives   : byte;
  DriveStr : array[4..26] of pchar;

Procedure AddDisk(const path:string);
begin
  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives],StrLen(DriveStr[Drives])+1);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  inc(Drives);
  if Drives>26 then
   Drives:=4;
end;


Function DiskFree(Drive: Byte): int64;
  (*
var
  fs : tstatfs;
  *)
Begin
  (* TODO fix
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and statfs(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and statfs(StrPas(drivestr[drive]),fs)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
  *)
End;



Function DiskSize(Drive: Byte): int64;
  (*
var
  fs : tstatfs;
  *)
Begin
  (* TODO fix
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and statfs(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and statfs(StrPas(drivestr[drive]),fs)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;
  *)
End;

Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   ChDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   MkDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
   RmDir(Dir);
  {$I+}
  result := (IOResult = 0);
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
//TODO fix
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
  (* TODO fix
  Unix.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second);
  Unix.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
  SystemTime.MilliSecond := 0;
  *)
end ;


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitInternationalGeneric;
  InitAnsi;  
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
  (* TODO fix
  Result:=StrError(ErrorCode);
  *)
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  (* TODO fix
  Result:=StrPas(Unix.Getenv(PChar(EnvVar)));
  *)
end;

Function GetEnvironmentVariableCount : Integer;

begin
  // Result:=FPCCountEnvVar(EnvP);
  Result:=0;
end;

Function GetEnvironmentString(Index : Integer) : String;

begin
  // Result:=FPCGetEnvStrFromP(Envp,Index);
  Result:='';
end;

function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
var
  s: AnsiString;
  wdpath: AnsiString;
  laststatuscode : longint;
Begin
  {Make ToolServers working directory in sync with our working directory}
  PathArgToFullPath(':', wdpath);
  wdpath:= 'Directory ' + wdpath;
  Result := ExecuteToolserverScript(PChar(wdpath), laststatuscode);
    {TODO Only change path when actually needed. But this requires some
     change counter to be incremented each time wd is changed. }

  s:= path + ' ' + comline;

  Result := ExecuteToolserverScript(PChar(s), laststatuscode);
  if Result = afpItemNotFound then
    Result := 900
  else
    Result := MacOSErr2RTEerr(Result);
  if Result <> 0
  then
    raise EOSErr;
  //TODO Better dos error codes
  if laststatuscode <> 0 then
    begin
      {MPW status might be 24 bits}
      Result := laststatuscode and $ffff;
      if Result = 0 then
        Result := 1;
    end
  else
    Result := 0;
End;

function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array Of AnsiString):integer;
begin
end;

procedure Sleep(milliseconds: Cardinal);
begin
end;

(*
Function GetLastOSError : Integer;

begin
end;
*)

{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
Finalization
  DoneExceptions;
end.
