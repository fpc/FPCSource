{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for POSIX compliant systems

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



{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }

Procedure AddDisk(const path:string);



implementation

uses dos,posix;

{ Include platform independent implementation part }
{$i sysutils.inc}



{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}
const
     { read/write permission for everyone }
     MODE_OPEN = S_IWUSR OR S_IRUSR OR
                 S_IWGRP OR S_IRGRP OR
                 S_IWOTH OR S_IROTH;


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

Var Flags : cint;
    FileHandle : cint;
{    lock: flock;}
BEGIN
  Flags:=0;
  Case (Mode and 3) of
    fmOpenRead : Flags:=Flags or O_RDONLY;
    fmOpenWrite : Flags:=Flags or O_WRONLY;
    fmOpenReadWrite : Flags:=Flags or O_RDWR;
  end;
  FileHandle:=sys_Open (pchar(FileName),Flags,MODE_OPEN);
  if (ErrNo=Sys_EROFS) and ((Flags and O_RDWR)<>0) then
   begin
     Flags:=Flags and not(O_RDWR);
     FileHandle:=sys_open(pchar(FileName),Flags,MODE_OPEN);
   end;
  FileOpen := longint(FileHandle);
(*
  { if there was an error, then don't do anything }
  if FileHandle = -1 then
     exit;
  { now check if the file can actually be used }
  { by verifying the locks on the file         }
  lock.l_whence := SEEK_SET;
  lock.l_start := 0; { from start of file }
  lock.l_len := 0;   { to END of file    }
  if sys_fcntl(FileHandle, F_GETLK, @lock)<>-1 then
    begin
        { if another process has created a lock on this file }
        { exclusive lock? }
        if (lock.l_type = F_WRLCK) then
           begin
             { close and exit }
             sys_close(FileHandle);
             FileOpen := -1;
             exit;
           end;
        { shared lock? }
        if (lock.l_type = F_RDLK) and
          ((Flags = O_RDWR) or Flags = O_WRONLY)) then
           begin
             { close and exit }
             sys_close(FileHandle);
             FileOpen := -1;
             exit;
           end;
    end;
  { now actually set the lock: }
  { only the following are simulated with sysutils : }
  {  - fmShareDenywrite (get exclusive lock)         }
  {  - fmShareExclusive (get exclusive lock)         }
  if ((Mode and fmShareDenyWrite)<>0) or
     ((Mode and fmShareExclusive)<>0) then
    begin
      lock.l_whence := SEEK_SET;
      lock.l_start := 0; { from stat of file    }
      lock.l_len := 0;   { to END of file       }
      lock.l_type := F_WRLCK;  { exclusive lock }
      if sys_fcntl(FileHandle, F_SETLK, @lock)=-1 then
        begin
          sys_close(FileHandel);
          FileOpen := -1;
          exit;
        end;
    end;
*)
end;


Function FileCreate (Const FileName : String) : Longint;

begin
  FileCreate:=sys_Open(pchar(FileName),O_RDWR or O_CREAT or O_TRUNC,MODE_OPEN);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

begin
  repeat
    FileRead:=sys_read(Handle,pchar(@Buffer),Count);
  until ErrNo<>Sys_EINTR;
  If FileRead = -1 then
    FileRead := 0;
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;

begin
  repeat
    FileWrite:=sys_write(Handle,pchar(@Buffer),Count);
  until ErrNo<>Sys_EINTR;
  if FileWrite = -1 then
    FileWrite := 0;
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;
var
 whence : cint;
begin
  FileSeek := -1;
  case Origin of
  { from beginning of file }
  0 :  whence := SEEK_SET;
  { from current position }
  1 :  whence := SEEK_CUR;
  { from end of file       }
  2 :  whence := SEEK_END;
  else
   exit;
  end;
  FileSeek := sys_lseek(Handle,FOffset,whence);
  if errno <> 0 then
   FileSeek := -1;
end;


Procedure FileClose (Handle : Longint);

begin
  sys_close(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  if sys_ftruncate(Handle,Size)=0 then
    FileTruncate := true
  else
    FileTruncate := false;
end;


Function FileAge (Const FileName : String): Longint;

var F: file;
    Time: longint;
begin
   Assign(F,FileName);
   Reset(F,1);
   dos.GetFTime(F,Time);
   Close(F);
   FileAge := Time;
end;


Function FileExists (Const FileName : String) : Boolean;

Var Info : Stat;

begin
  if sys_stat(pchar(filename),Info)<>0 then
    FileExists := false
  else
    FileExists := true;
end;


Function UNIXToWinAttr (FN : Pchar; Const Info : Stat) : Longint;

begin
  Result:=faArchive;
  If S_ISDIR(Info.st_mode) then
    Result:=Result or faDirectory ;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  if (info.st_mode and S_IWUSR)=0 then
    Result:=Result or fareadonly;
  If S_ISREG(Info.st_Mode) Then
     Result:=Result or faSysFile;
end;




type 
  PDOSSearchRec = ^SearchRec;

Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

Const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
var
  p : pDOSSearchRec;
  dosattr: word;
begin
 dosattr:=0;
 if Attr and faHidden <> 0 then
   dosattr := dosattr or Hidden;
 if Attr and faSysFile <> 0 then
   dosattr := dosattr or SysFile;
 if Attr and favolumeID <> 0 then
   dosattr := dosattr or VolumeID;
 if Attr and faDirectory <> 0 then
   dosattr := dosattr or faDirectory;
 New(p);
 Rslt.FindHandle :=  THandle(p);
 dos.FindFirst(path,dosattr,p^);
 if DosError <> 0 then
    begin
      FindFirst := -1;
    end
 else
   begin
     Rslt.Name := p^.Name;
     Rslt.Time := p^.Time;
     Rslt.Attr := p^.Attr;
     Rslt.ExcludeAttr := not p^.Attr;
     Rslt.Size := p^.Size;
     FindFirst := 0;
   end;   
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
var
 p : pDOSSearchRec;
begin
  p:= PDOsSearchRec(Rslt.FindHandle);
  if not assigned(p) then
     begin
       FindNext := -1;
       exit;
     end;
  Dos.FindNext(p^);
 if DosError <> 0 then
    begin
      FindNext := -1;
    end
 else
   begin
     Rslt.Name := p^.Name;
     Rslt.Time := p^.Time;
     Rslt.Attr := p^.Attr;
     Rslt.ExcludeAttr := not p^.Attr;
     Rslt.Size := p^.Size;
     FindNext := 0;
   end;     
end;


Procedure FindClose (Var F : TSearchrec);

Var
  p : PDOSSearchRec;

begin
  p:=PDOSSearchRec(f.FindHandle); 
  if not assigned(p) then
       exit;
  Dos.FindClose(p^);
  if assigned(p) then
     Dispose(p);
  f.FindHandle := THandle(nil);
end;

Function FileGetDate (Handle : Longint) : Longint;

Var Info : Stat;

begin
  If sys_FStat(Handle,Info)<>0 then
    Result:=-1
  else
    Result:=Info.st_mtime;
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  // Impossible under unix from FileHandle !!
  FileSetDate:=-1;
end;


Function FileGetAttr (Const FileName : String) : Longint;

Var Info : Stat;

begin
  If sys_stat (pchar(FileName),Info)<>0 then
    Result:=-1
  Else
    Result:=UNIXToWinAttr(Pchar(FileName),Info);
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : String) : Boolean;
begin
  if sys_unlink(pchar(FileName))=0 then
    DeleteFile := true
  else
    DeleteFile := false;
end;

Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  { you can directly typecast and ansistring to a pchar }
  if sys_rename(pchar(OldName),pchar(NewName))=0 then
    RenameFile := TRUE
  else
    RenameFile := FALSE;
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
Begin
  DiskFree := dos.diskFree(Drive);
End;



Function DiskSize(Drive: Byte): int64;
Begin
  DiskSize := dos.DiskSize(Drive);
End;




Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
   ChDir(NewDir);
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
   MkDir(NewDir);
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
   RmDir(Dir);
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
var
 dayOfWeek: word;
begin
  dos.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second,SystemTime.Millisecond);
  dos.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day, DayOfWeek);
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
{  Result:=StrError(ErrorCode);}
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=Dos.Getenv(shortstring(EnvVar));
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
    Revision 1.2  2002-08-10 13:42:36  marco
     * Fixes Posix dir copied to devel branch

    Revision 1.1.2.5  2002/04/28 07:28:43  carl
    * some cleanup

    Revision 1.1.2.4  2002/03/03 08:47:37  carl
    + FindFirst / FindNext implemented

    Revision 1.1.2.3  2002/01/22 07:41:11  michael
    + Fixed FileSearch bug in Win32 and made FIleSearch platform independent

    Revision 1.1.2.2  2001/09/29 20:16:53  carl
    * bugfix of read/write wrong address was passed as parameter

    Revision 1.1.2.1  2001/08/15 01:07:07  carl
    + first version of sysutils


}