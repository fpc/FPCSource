{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for linux

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
  Unix,errors;

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileOpen:=fdOpen (FileName,LinuxFlags);
  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=fdOpen(FileName,Open_RdWr or Open_Creat or Open_Trunc);
end;


Function FileCreate (Const FileName : String;Mode : Longint) : Longint;

Var LinuxFlags : longint;

BEGIN
  LinuxFlags:=0;
  Case (Mode and 3) of
    0 : LinuxFlags:=LinuxFlags or Open_RdOnly;
    1 : LinuxFlags:=LinuxFlags or Open_WrOnly;
    2 : LinuxFlags:=LinuxFlags or Open_RdWr;
  end;
  FileCreate:=fdOpen(FileName,LinuxFlags or Open_Creat or Open_Trunc);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=fdRead (Handle,Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=fdWrite (Handle,Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=fdSeek (Handle,FOffset,Origin);
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;

begin
  {$warning need to add 64bit call }
  FileSeek:=fdSeek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  fdclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=fdtruncate(Handle,Size);
end;

Function FileAge (Const FileName : String): Longint;

Var Info : Stat;
    Y,M,D,hh,mm,ss : word;

begin
  If not fstat (FileName,Info) then
    exit(-1)
  else
    begin
    EpochToLocal(info.mtime,y,m,d,hh,mm,ss);
    Result:=DateTimeToFileDate(EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0));
    end;
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  FileExists:=fstat(filename,Info);
end;


Function DirectoryExists (Const DirName : String) : Boolean;

Var Info : Stat;

begin
  DirectoryExists:=fstat(dirname,Info) and
                   ((info.mode and STAT_IFMT)=STAT_IFDIR);
end;


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

Function DoFind(Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  Result:=-1;
  GlobSearchRec:=PGlobSearchRec(Rslt.FindHandle);
  If (GlobSearchRec^.GlobHandle<>Nil) then
    While (GlobSearchRec^.GlobHandle<>Nil) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;



Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRec;

begin
  New(GlobSearchRec);
  GlobSearchRec^.Path:=ExpandFileName(ExtractFilePath(Path));
  GlobSearchRec^.GlobHandle:=Glob(Path);
  Rslt.ExcludeAttr:=Not Attr; //!! Not correct !!
  Rslt.FindHandle:=Longint(GlobSearchRec);
  Result:=DoFind (Rslt);
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  Result:=DoFind (Rslt);
end;


Procedure FindClose (Var F : TSearchrec);

Var
  GlobSearchRec : PGlobSearchRec;

begin
  GlobSearchRec:=PGlobSearchRec(F.FindHandle);
  GlobFree (GlobSearchRec^.GlobHandle);
  Dispose(GlobSearchRec);
end;


Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If Not(FStat(Handle,Info)) then
    Result:=-1
  else
    Result:=Info.Mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under Linux from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If Not FStat (FileName,Info) then
    Result:=-1
  Else
    Result:=LinuxToWinAttr(Pchar(FileName),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:=UnLink (FileName);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=Unix.FRename(OldNAme,NewName);
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
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and statfs(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and statfs(StrPas(drivestr[drive]),fs)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;
End;



Function DiskSize(Drive: Byte): int64;
var
  fs : tstatfs;
Begin
  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and statfs(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and statfs(StrPas(drivestr[drive]),fs)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;
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
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
  Unix.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second);
  Unix.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
  SystemTime.MilliSecond := 0;
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
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
  Result:=StrError(ErrorCode);
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=StrPas(Unix.Getenv(PChar(EnvVar)));
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
Finalization
  DoneExceptions;
end.
{

  $Log$
  Revision 1.15  2003-03-28 19:06:59  peter
    * directoryexists added

  Revision 1.14  2003/01/03 20:41:04  peter
    * FileCreate(string,mode) overload added

  Revision 1.13  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

  Revision 1.12  2002/01/25 16:23:03  peter
    * merged filesearch() fix

}
