{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Sysutils unit for netware (libc)

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

uses Libc,DOS;


TYPE
  TNetwareLibcFindData =
  RECORD
    DirP  : Pdirent;               { used for opendir }
    EntryP: Pdirent;               { and readdir }
    Magic : WORD;                  { to avoid abends with uninitialized TSearchRec }
  END;

{ Include platform independent interface part }
{$i sysutilh.inc}



{ additional NetWare file flags}
CONST
  faSHARE              = $00000080;  { Sharable file                   }

  faNO_SUBALLOC        = $00000800;  { Don't sub alloc. this file      }
  faTRANS              = $00001000;  { Transactional file (TTS usable) }
  faREADAUD            = $00004000;  { Read audit                      }
  faWRITAUD            = $00008000;  { Write audit                     }

  faIMMPURG            = $00010000;  { Immediate purge                 }
  faNORENAM            = $00020000;  { Rename inhibit                  }
  faNODELET            = $00040000;  { Delete inhibit                  }
  faNOCOPY             = $00080000;  { Copy inhibit                    }

  faFILE_MIGRATED      = $00400000;  { File has been migrated          }
  faDONT_MIGRATE       = $00800000;  { Don't migrate this file         }
  faIMMEDIATE_COMPRESS = $02000000;  { Compress this file immediately  }
  faFILE_COMPRESSED    = $04000000;  { File is compressed              }
  faDONT_COMPRESS      = $08000000;  { Don't compress this file        }
  faCANT_COMPRESS      = $20000000;  { Can't compress this file        }
  faATTR_ARCHIVE       = $40000000;  { Entry has had an EA modified,   }
                                     { an ownerID changed, or trustee  }
                                     { info changed, etc.              }



implementation

  uses
    sysconst;

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
VAR NWOpenFlags : longint;
BEGIN
  NWOpenFlags:=0;
  Case (Mode and 3) of
    0 : NWOpenFlags:=NWOpenFlags or O_RDONLY;
    1 : NWOpenFlags:=NWOpenFlags or O_WRONLY;
    2 : NWOpenFlags:=NWOpenFlags or O_RDWR;
  end;
  FileOpen := open (pchar(FileName),NWOpenFlags);

  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=open(Pchar(FileName),O_RdWr or O_Creat or O_Trunc);
end;

Function FileCreate (Const FileName : String; mode:longint) : Longint;

begin
  FileCreate:=FileCreate (FileName);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=libc.fpread (Handle,@Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=libc.fpwrite (Handle,@Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=libc.fplseek (Handle,FOffset,Origin);
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;
begin
  FileSeek:=libc.fplseek (Handle,FOffset,Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  libc.fpclose(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=(libc.fpchsize(Handle,Size) = 0);
end;

Function FileLock (Handle,FOffset,FLen : Longint) : Longint;
begin
  {$warning FileLock not implemented}
  //FileLock := _lock (Handle,FOffset,FLen);
end;

Function FileLock (Handle : Longint; FOffset,FLen : Int64) : Longint;
begin
  {$warning need to add 64bit FileLock call }
  //FileLock := FileLock (Handle, longint(FOffset),longint(FLen));
end;

Function FileUnlock (Handle,FOffset,FLen : Longint) : Longint;
begin
  //FileUnlock := _unlock (Handle,FOffset,FLen);
  {$warning FileUnLock not implemented}
end;

Function FileUnlock (Handle : Longint; FOffset,FLen : Int64) : Longint;
begin
  {$warning need to add 64bit FileUnlock call }
  //FileUnlock := FileUnlock (Handle, longint(FOffset),longint(FLen));
end;

Function FileAge (Const FileName : String): Longint;

VAR Info : TStat;
    _PTM  : PTM;
begin
  If stat (pchar(FileName),Info) <> 0 then
    exit(-1)
  else
    begin
      _PTM := localtime (Info.st_mtim.tv_sec);
      IF _PTM = NIL THEN
        exit(-1)
      else
        WITH _PTM^ DO
          Result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
    end;
end;


Function FileExists (Const FileName : String) : Boolean;
VAR Info : TStat;
begin
  FileExists:=(stat(pchar(filename),Info) = 0);
end;



PROCEDURE find_setfields (VAR f : TsearchRec);
VAR T : Dos.DateTime;
BEGIN
  WITH F DO
  BEGIN
    IF FindData.Magic = $AD01 THEN
    BEGIN
      {attr := FindData.EntryP^.d_attr AND $FF;}  // lowest 8 bit -> same as dos
      attr := FindData.EntryP^.d_flags;   { return complete netware attributes }
      //!!UnpackTime(FindData.EntryP^.d_time + (LONGINT (FindData.EntryP^.d_date) SHL 16), T);
      //!!time := DateTimeToFileDate(EncodeDate(T.Year,T.Month,T.day)+EncodeTime(T.Hour,T.Min,T.Sec,0));
      size := FindData.EntryP^.d_size;
      name := strpas (FindData.EntryP^.d_name);
    END ELSE
    BEGIN
      FillChar (f,SIZEOF(f),0);
    END;
  END;
END;



Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;
begin
  IF path = '' then
    exit (18);
  Rslt.FindData.DirP := opendir (pchar(Path));
  IF Rslt.FindData.DirP = NIL THEN
    exit (18);
  //!!IF attr <> faAnyFile THEN
  //!!  _SetReaddirAttribute (Rslt.FindData.DirP, attr);
  Rslt.FindData.Magic := $AD01;
  Rslt.FindData.EntryP := readdir (Rslt.FindData.DirP);
  if Rslt.FindData.EntryP = nil then
  begin
    closedir (Rslt.FindData.DirP);
    Rslt.FindData.DirP := NIL;
    result := 18;
  end else
  begin
    find_setfields (Rslt);
    result := 0;
  end;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  if Rslt.FindData.Magic <> $AD01 then
    exit (18);
  Rslt.FindData.EntryP := readdir (Rslt.FindData.DirP);
  if Rslt.FindData.EntryP = nil then
    exit (18);
  find_setfields (Rslt);
  result := 0;
end;


Procedure FindClose (Var F : TSearchrec);
begin
  if F.FindData.Magic = $AD01 then
  begin
    if F.FindData.DirP <> nil then
      closedir (F.FindData.DirP);
    F.FindData.Magic := 0;
    F.FindData.DirP := NIL;
    F.FindData.EntryP := NIL;
  end;
end;


Function FileGetDate (Handle : Longint) : Longint;
Var Info : TStat;
    _PTM  : PTM;
begin
  If fstat(Handle,Info) <> 0 then
    Result:=-1
  else
    begin
      _PTM := localtime (Info.st_mtim.tv_sec);
      IF _PTM = NIL THEN
        exit(-1)
      else
        with _PTM^ do
          Result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
    end;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
begin
  { i think its impossible under netware from FileHandle. I dident found a way to get the
    complete pathname of a filehandle, that would be needed for ChangeDirectoryEntry }
  FileSetDate:=-1;
  ConsolePrintf ('warning: fpc sysutils.FileSetDate not implemented'#13#10);
  {$warning FileSetDate not implemented (i think is impossible) }
end;


Function FileGetAttr (Const FileName : String) : Longint;
Var Info : TStat;
begin
  If stat (pchar(FileName),Info) <> 0 then
    Result:=-1
  Else
    Result := Info.st_flags AND $FFFF;
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
//VAR MS : NWModifyStructure;
begin
  {FillChar (MS, SIZEOF (MS), 0);
  if _ChangeDirectoryEntry (PChar (Filename), MS, MFileAtrributesBit, 0) <> 0 then
    result := -1
  else
    result := 0;}
{$warning FileSetAttr needs implementation}
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:= (libc.UnLink (pchar(FileName)) = 0);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=(libc.rename(pchar(OldName),pchar(NewName)) = 0);
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
    'a:.',
    'b:.',
    'sys:/'
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
//var fs : statfs;
Begin
{  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;}
  DiskFree := -1;
  ConsolePrintf ('warning: fpc sysutils.diskfree not implemented'#13#10);
  {$warning DiskFree not implemented (does it make sense ?) }
End;



Function DiskSize(Drive: Byte): int64;
//var fs : statfs;
Begin
{  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   DiskSize:=int64(fs.blocks)*int64(fs.bsize)
  else
   DiskSize:=-1;}
  DiskSize := -1;
  ConsolePrintf ('warning: fpc sysutils.disksize not implemented'#13#10);
  {$warning DiskSize not implemented (does it make sense ?) }
End;


Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  Libc.FpChDir(pchar(NewDir));
  result := (___errno^ = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  Libc.FpMkDir(pchar(NewDir),0);
  result := (___errno^ = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  libc.FpRmDir(pchar(Dir));
  result := (___errno^ = 0);
end;


function DirectoryExists (const Directory: string): boolean;
var Info : TStat;
begin
  If stat (pchar(Directory),Info) <> 0 then
    exit(false)
  else
    Exit ((Info.st_mode and M_A_SUBDIR) <> 0);
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
  RingBell;
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
var xx : word;
begin
  Dos.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, xx);
  Dos.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day, xx);
  SystemTime.MilliSecond := 0;
end;


Procedure InitAnsi;
Var i : longint;
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
  Result:='';  // StrError(ErrorCode);
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=StrPas(libc.getenv(PChar(EnvVar)));
end;


function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;

var
  e : EOSError;
  CommandLine: AnsiString;

begin
  dos.exec(path,comline);

  if (Dos.DosError <> 0) then
    begin
      if ComLine <> '' then
       CommandLine := Path + ' ' + ComLine
      else
       CommandLine := Path;
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,Dos.DosError]);
      e.ErrorCode:=Dos.DosError;
      raise e;
    end;
  Result := DosExitCode;
end;


function ExecuteProcess (const Path: AnsiString;
                                  const ComLine: array of AnsiString): integer;

var
  CommandLine: AnsiString;
  I: integer;

begin
  Commandline := '';
  for I := 0 to High (ComLine) do
   if Pos (' ', ComLine [I]) <> 0 then
    CommandLine := CommandLine + ' ' + '"' + ComLine [I] + '"'
   else
    CommandLine := CommandLine + ' ' + Comline [I];
  ExecuteProcess := ExecuteProcess (Path, CommandLine);
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
  Revision 1.1  2004-09-05 20:58:47  armin
  * first rtl version for netwlibc

}
