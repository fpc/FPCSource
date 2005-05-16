{
    $Id: dos_beos.inc,v 1.2 2005/02/14 17:13:21 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by members of the Free Pascal
    development team

    Operating system specific calls for DOS unit (part of POSIX interface)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i syscall.inc}
{$i beos.inc}

{$define DOS_HAS_EXEC}


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
  The drive names are OS specific
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',            { the current directory }
    '/disk 0/.',    { mounted floppy 1 }
    '/disk 1/.',    { mounted floppy 2 }
    '/boot/.'       { the boot up disk }
    );


Function DosVersion:Word;
Begin
  DosVersion := 0;
End;



Function DiskFree(Drive: Byte): int64;
var
  info: fs_info;
  device : dev_t;
Begin
  device := 0;
  DiskFree := -1;
  if (Drive < 4) and (FixDriveStr[Drive]<>nil) then
    begin
     device:= dev_for_path(FixDriveStr[Drive]);
    end
  else
  if (Drive>4) and (Drive<=MAX_DRIVES) and (drivestr[Drive]<>nil) then
     device := dev_for_path(driveStr[drive])
  else
     begin
       exit;
     end;
  if fs_Stat_dev(device,info)=0 then
    DiskFree := int64(info.block_size)*int64(info.free_blocks);
End;



Function DiskSize(Drive: Byte): int64;
var
  info: fs_info;
  device : dev_t;
Begin
  device := 0;
  DiskSize:= -1;
  if (Drive < 4) and (FixDriveStr[Drive]<>nil) then
    begin
     device:= dev_for_path(FixDriveStr[Drive]);
    end
  else
  if (Drive>4) and (Drive<=MAX_DRIVES) and (drivestr[Drive]<>nil) then
     device := dev_for_path(driveStr[drive])
  else
     begin
       exit;
     end;
  if fs_Stat_dev(device,info)=0 then
    DiskSize := int64(info.block_size)*int64(info.total_blocks);
End;



{******************************************************************************
                               --- Exec ---
******************************************************************************}
Procedure Exec(const path: pathstr; const comline: comstr);
var p:string;
    argv:ppchar;
    argc:longint;
    th:thread_id;
    status : status_t;
begin
  LastDosExitCode:=0;
  DosError:= 0;
  p:=path+' '+comline;
  argv:=StringToPPChar(p,argc);
  th:=load_image(argc,argv,system.envp);
  if th<0 then begin
    DosError:=5;  { lets emulate an error }
    exit;
  end;
  wait_for_thread(th,status);
  LastDosExitCode:=status and $FF; { only keep the lower 8-bits }
end;


function GetTimeZoneString : string;
begin
  GetTimeZoneString:=getenv('TZ');
end;

function GetTimezoneFile:string;
var
  f,len : longint;
  s : string;
  info : stat;
  buffer : array[0..MAXPATHLEN+1] of char;
begin
  GetTimezoneFile:='';

  if kget_tzfilename(pchar(@buffer))=0 then
  begin
     GetTimeZoneFile := strpas(pchar(@buffer));
  end;
end;


{
 $Log: dos_beos.inc,v $
 Revision 1.2  2005/02/14 17:13:21  peter
   * truncate log

}
