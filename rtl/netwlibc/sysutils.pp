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
    DirP  : Pdirent;          { used for opendir }
    EntryP: Pdirent;          { and readdir }
    Magic : longint;          { to avoid abends with uninitialized TSearchRec }
    _mask : string;           { search mask i.e. *.* }
    _dir  : string;           { directory where to search }
    _attr : longint;          { specified attribute }
    fname : string;           { full pathname of found file }
  END;

{ Include platform independent interface part }
{$i sysutilh.inc}



{ additional NetWare file flags}
CONST
  faSHARE              = M_A_SHARE shr 16;              // Sharable file

  //faNO_SUBALLOC        = $00000800;                   // Don't sub alloc. this file
  faTRANS              = M_A_TRANS shr 16;              // Transactional file (TTS usable)
  //faREADAUD            = $00004000;                   // clib only: Read audit
  //faWRITAUD            = $00008000;                   // clib only: Write audit

  faIMMPURG            = M_A_IMMPURG shr 16;            // Immediate purge
  faNORENAM            = M_A_NORENAM shr 16;            // Rename inhibit
  faNODELET            = M_A_NODELET shr 16;            // Delete inhibit
  faNOCOPY             = M_A_NOCOPY shr 16;             // Copy inhibit

  //faFILE_MIGRATED      = $00400000;                   // clib only: File has been migrated
  //faDONT_MIGRATE       = $00800000;                   // clib only: Don't migrate this file
  faIMMEDIATE_COMPRESS = M_A_IMMCOMPRESS shr 16;        // Compress this file immediately
  faFILE_COMPRESSED    = M_A_FILE_COMPRESSED shr 16;    // File is compressed
  faDONT_COMPRESS      = M_A_DONT_COMPRESS shr 16;      // Don't compress this file
  faCANT_COMPRESS      = M_A_CANT_COMPRESS shr 16;      // Can't compress this file
  //faATTR_ARCHIVE       = $40000000;                   // clib only: Entry has had an EA modified,
                                                        // an ownerID changed, or trustee
                                                        // info changed, etc.
  faSetNetwareAttrs    = M_A_BITS_SIGNIFICANT;          // if this is set, netware flags are changed also



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
  FileOpen := Fpopen (pchar(FileName),NWOpenFlags);

  //!! We need to set locking based on Mode !!
end;


Function FileCreate (Const FileName : String) : Longint;
begin
  FileCreate:=Fpopen(Pchar(FileName),O_RdWr or O_Creat or O_Trunc or O_Binary);
  if FileCreate >= 0 then
    FileSetAttr (Filename, 0);  // dont know why but open always sets ReadOnly flag
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
  FileSeek:=libc.fplseek64 (Handle,FOffset,Origin);
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
var Info : TStat;
    TM  : TTM;
begin
  If stat (pchar(FileName),Info) <> 0 then
    exit(-1)
  else
    begin
      localtime_r (Info.st_mtim.tv_sec,tm);
      with TM do
        result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
    end;
end;


Function FileExists (Const FileName : String) : Boolean;
VAR Info : TStat;
begin
  FileExists:=(stat(pchar(filename),Info) = 0);
end;


(*
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
END;*)


Function UnixToWinAge(UnixAge : time_t): Longint;
Var tm : TTm;
begin
  libc.localtime_r (UnixAge, tm);
  with tm do
    Result:=DateTimeToFileDate(EncodeDate(tm_year+1900,tm_mon+1,tm_mday)+EncodeTime(tm_hour,tm_min,tm_sec,0));
end;


{returns true if attributes match}
function find_setfields (var f : TsearchRec; var AttrsOk : boolean) : longint;
var
  StatBuf : TStat;
  fname   : string;
begin
  result := 0;
  with F do
  begin
    if FindData.Magic = $AD02 then
    begin
      attr := (Pdirent(FindData.EntryP)^.d_mode shr 16) and $ffff;
      size := Pdirent(FindData.EntryP)^.d_size;
      name := strpas (Pdirent(FindData.EntryP)^.d_name);
      fname := FindData._dir + name;
      if stat (pchar(fname),StatBuf) = 0 then
        time := UnixToWinAge (StatBuf.st_mtim.tv_sec)
      else
        time := 0;
      AttrsOk := false;
      if (f.FindData._attr and faHidden) = 0 then
        if attr and faHidden > 0 then exit;
      if (f.FindData._attr and faDirectory) = 0 then
        if attr and faDirectory > 0 then exit;
      if (f.FindData._attr and faSysFile) = 0 then
        if attr and faSysFile > 0 then exit;
      AttrsOk := true;
    end else
    begin
      FillChar (f,sizeof(f),0);
      result := 18;
    end;
  end;
end;



(*
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
end;*)
function findfirst(const path : string;attr : longint;var Rslt : TsearchRec) : longint;
var
  path0 : string;
  p     : longint;
begin
  IF path = '' then
  begin
    result := 18;
    exit;
  end;
  Rslt.FindData._attr := attr;
  p := length (path);
  while (p > 0) and (not (path[p] in ['\','/'])) do
    dec (p);
  if p > 0 then
  begin
    Rslt.FindData._mask := copy (path,p+1,255);
    Rslt.FindData._dir := copy (path,1,p);
  end else
  begin
    Rslt.FindData._mask := path;
    Rslt.FindData._dir := GetCurrentDir;
    if (Rslt.FindData._dir[length(Rslt.FindData._dir)] <> '/') and
       (Rslt.FindData._dir[length(Rslt.FindData._dir)] <> '\') then
      Rslt.FindData._dir := Rslt.FindData._dir + '/';
  end;
  if Rslt.FindData._mask = '*' then Rslt.FindData._mask := '';
  if Rslt.FindData._mask = '*.*' then Rslt.FindData._mask := '';
  //writeln (stderr,'mask: "',Rslt._mask,'" dir:"',path0,'"');
  Pdirent(Rslt.FindData.DirP) := opendir (pchar(Rslt.FindData._dir));
  if Rslt.FindData.DirP = nil then
    result := 18
  else begin
    Rslt.FindData.Magic := $AD02;
    result := findnext (Rslt);
  end;
end;


function findnext(var Rslt : TsearchRec) : longint;
var attrsOk : boolean;
begin
  if Rslt.FindData.Magic <> $AD02 then
  begin
    result := 18;
    exit;
  end;
  result:=0;
  repeat
    Pdirent(Rslt.FindData.EntryP) := readdir (Pdirent(Rslt.FindData.DirP));
    if Rslt.FindData.EntryP = nil then
      result := 18
    else
    result := find_setfields (Rslt,attrsOk);
    if (result = 0) and (attrsOk) then
    begin
      if Rslt.FindData._mask = #0 then exit;
      if fnmatch(@Rslt.FindData._mask[1],Pdirent(Rslt.FindData.EntryP)^.d_name,FNM_CASEFOLD) = 0 then
        exit;
    end;
  until result <> 0;
end;


Procedure FindClose(Var f: TSearchRec);
begin
  if F.FindData.Magic <> $AD02 then exit;
  doserror:=0;
  closedir (Pdirent(f.FindData.DirP));
  FillChar (f,sizeof(f),0);
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
Begin
  {dont know how to do that, utime needs filename}
  result := -1;
end;


Function FileGetAttr (Const FileName : String) : Longint;
Var Info : TStat;
begin
  If stat (pchar(FileName),Info) <> 0 then
    Result:=-1
  Else
    Result := (Info.st_mode shr 16) and $ffff;
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
var
  StatBuf : TStat;
  newMode : longint;
begin
  if stat (pchar(Filename),StatBuf) = 0 then
  begin
    {what should i do here ?
     only support sysutils-standard attributes or also support the extensions defined
     only for netware libc ?
     For now i allow the complete attributes if the bit faSetNetwareAttrs is set. Otherwise
     only the standard attributes can be modified}
    if attr and faSetNetwareAttrs > 0 then
    begin
      newmode := ((attr shl 16) and $ffff0000) or M_A_BITS_SIGNIFICANT;
    end else
    begin
      attr := (attr and $2f) shl 16;
      newmode := StatBuf.st_mode and ($ffff0000-M_A_RDONLY-M_A_HIDDEN- M_A_SYSTEM-M_A_SUBDIR-M_A_ARCH);
      newmode := newmode or (attr shl 16) or M_A_BITS_SIGNIFICANT;
    end;
    if chmod (pchar(Filename),newMode) < 0 then
      result := ___errno^ else
      result := 0;
  end else
    result := ___errno^;
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
//var fs : Tstatfs;
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
  Revision 1.2  2004-09-12 20:51:22  armin
  * added keyboard and video
  * a lot of fixes

  Revision 1.1  2004/09/05 20:58:47  armin
  * first rtl version for netwlibc

}
