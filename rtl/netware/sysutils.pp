{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for netware

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

uses DOS;

{$I nwsys.inc}
{$I errno.inc}

TYPE
  TNetwareFindData =
  RECORD
    DirP  : PNWDirEnt;               { used for opendir }
    EntryP: PNWDirEnt;               { and readdir }
    Magic : WORD;                    { to avoid abends with uninitialized TSearchRec }
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
  FileOpen := _open (pchar(FileName),NWOpenFlags,0);

  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=_open(Pchar(FileName),O_RdWr or O_Creat or O_Trunc,0);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  FileRead:=_read (Handle,@Buffer,Count);
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  FileWrite:=_write (Handle,@Buffer,Count);
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;

begin
  FileSeek:=_lseek (Handle,FOffset,Origin);
end;


Function FileSeek (Handle : Longint; FOffset,Origin : Int64) : Int64;
begin
  {$warning need to add 64bit call }
  FileSeek:=FileSeek(Handle,Longint(FOffset),Longint(Origin));
end;


Procedure FileClose (Handle : Longint);

begin
  _close(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  FileTruncate:=(_chsize(Handle,Size) = 0);
end;

Function FileAge (Const FileName : String): Longint;

VAR Info : NWStatBufT;
    PTM  : PNWTM;
begin
  If _stat (pchar(FileName),Info) <> 0 then
    exit(-1)
  else
    begin
      PTM := _localtime (Info.st_mtime);
      IF PTM = NIL THEN
        exit(-1)
      else
        WITH PTM^ DO
          Result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
    end;
end;


Function FileExists (Const FileName : String) : Boolean;
VAR Info : NWStatBufT;
begin
  FileExists:=(_stat(pchar(filename),Info) = 0);
end;



PROCEDURE find_setfields (VAR f : TsearchRec);
VAR T : Dos.DateTime;
BEGIN
  WITH F DO
  BEGIN
    IF FindData.Magic = $AD01 THEN
    BEGIN
      {attr := FindData.EntryP^.d_attr AND $FF;}  // lowest 8 bit -> same as dos
      attr := FindData.EntryP^.d_attr;   { return complete netware attributes }
      UnpackTime(FindData.EntryP^.d_time + (LONGINT (FindData.EntryP^.d_date) SHL 16), T);
      time := DateTimeToFileDate(EncodeDate(T.Year,T.Month,T.day)+EncodeTime(T.Hour,T.Min,T.Sec,0));
      size := FindData.EntryP^.d_size;
      name := strpas (FindData.EntryP^.d_nameDOS);
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
  Rslt.FindData.DirP := _opendir (pchar(Path));
  IF Rslt.FindData.DirP = NIL THEN
    exit (18);
  IF attr <> faAnyFile THEN
    _SetReaddirAttribute (Rslt.FindData.DirP, attr);
  Rslt.FindData.Magic := $AD01;
  Rslt.FindData.EntryP := _readdir (Rslt.FindData.DirP);
  IF Rslt.FindData.EntryP = NIL THEN
  BEGIN
    _closedir (Rslt.FindData.DirP);
    Rslt.FindData.DirP := NIL;
    exit (18);
  END ELSE
  BEGIN
    find_setfields (Rslt);
    exit (0);
  END;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  IF Rslt.FindData.Magic <> $AD01 THEN
    exit (18);
  Rslt.FindData.EntryP := _readdir (Rslt.FindData.DirP);
  IF Rslt.FindData.EntryP = NIL THEN
    exit (18)
  ELSE
  BEGIN
    find_setfields (Rslt);
    exit (0);
  END;
end;


Procedure FindClose (Var F : TSearchrec);
begin
  IF F.FindData.Magic = $AD01 THEN
  BEGIN
    IF F.FindData.DirP <> NIL THEN
      _closedir (F.FindData.DirP);
    F.FindData.Magic := 0;
    F.FindData.DirP := NIL;
    F.FindData.EntryP := NIL;
  END;
end;


Function FileGetDate (Handle : Longint) : Longint;
Var Info : NWStatBufT;
    PTM  : PNWTM;
begin
  If _fstat(Handle,Info) <> 0 then
    Result:=-1
  else
    begin
      PTM := _localtime (Info.st_mtime);
      IF PTM = NIL THEN
        exit(-1)
      else
        WITH PTM^ DO
          Result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
    end;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
begin
  { i think its impossible under netware from FileHandle. I dident found a way to get the
    complete pathname of a filehandle, that would be needed for ChangeDirectoryEntry }
  FileSetDate:=-1;
  ConsolePrintf ('warning: fpc sysutils.FileSetDate not implemented'#13#10,0);
end;


Function FileGetAttr (Const FileName : String) : Longint;
Var Info : NWStatBufT;
begin
  If _stat (pchar(FileName),Info) <> 0 then
    Result:=-1
  Else
    Result := Info.st_attr AND $FFFF;
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
VAR MS : NWModifyStructure;
begin
  FillChar (MS, SIZEOF (MS), 0);
  if _ChangeDirectoryEntry (PChar (Filename), MS, MFileAtrributesBit, 0) <> 0 then
    exit (-1)
  else
    exit (0);
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  Result:= (_UnLink (pchar(FileName)) = 0);
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  RenameFile:=(_rename(pchar(OldName),pchar(NewName)) = 0);
end;

{ ad: 27 Feb 2002: now implemented globaly ??
Function FileSearch (Const Name, DirList : String) : String;
begin
  FileSearch:=Dos.FSearch(Name,Dirlist);
end;
}

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
//var fs : statfs;
Begin
{  if ((Drive<4) and (not (fixdrivestr[Drive]=nil)) and fsstat(StrPas(fixdrivestr[drive]),fs)) or
     ((not (drivestr[Drive]=nil)) and fsstat(StrPas(drivestr[drive]),fs)) then
   Diskfree:=int64(fs.bavail)*int64(fs.bsize)
  else
   Diskfree:=-1;}
  DiskFree := -1;
  ConsolePrintf ('warning: fpc sysutils.diskfree not implemented'#13#10,0);
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
  ConsolePrintf ('warning: fpc sysutils.disksize not implemented'#13#10,0);
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
  _RingTheBell;
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
  Result:=StrPas(_getenv(PChar(EnvVar)));
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
  Revision 1.5  2002-03-08 19:10:14  armin
  * added 64 bit fileseek (currently only 32 bit supported)

  Revision 1.4  2001/06/03 15:18:01  peter
    * eoutofmemory and einvalidpointer fix

  Revision 1.3  2001/04/16 18:39:50  florian
    * updates from Armin commited

  Revision 1.2  2001/04/11 14:17:00  florian
    * added logs, fixed email address of Armin, it is
      diehl@nordrhein.de

  Revision 1.1  2001/04/11 14:14:12  florian
    * initial commit, thanks to Armin Diehl (diehl@nordrhein.de)

  Revision 1.8  2001/02/20 22:19:38  peter
    * always test before commiting after merging, linux -> unix change

  Revision 1.7  2001/02/20 22:14:19  peter
    * merged getenvironmentvariable

  Revision 1.6  2001/01/21 20:21:40  marco
   * Rename fest II. Rtl OK

  Revision 1.5  2000/12/28 20:50:04  peter
    * merged fixes from 1.0.x

  Revision 1.4  2000/12/18 14:01:42  jonas
    * fixed constant range error

  Revision 1.3  2000/11/28 20:06:12  michael
  + merged fix for findfirst/findnext/findclose

  Revision 1.2  2000/09/18 13:14:51  marco
   * Global Linux +bsd to (rtl/freebsd rtl/unix rtl/linux structure)

  Revision 1.3  2000/08/29 17:58:13  michael
  Merged syserrormsg fix

  Revision 1.2  2000/08/20 15:46:46  peter
    * sysutils.pp moved to target and merged with disk.inc, filutil.inc
  Revision 1.1.2.2  2000/11/28 20:01:22  michael
    + Fixed findfirst/findnext/findclose

  Revision 1.1.2.1  2000/09/14 13:38:26  marco
    * Moved from Linux dir. now start of generic unix dir, from which the
      really exotic features should be moved to the target specific dirs.
}
