{
    $Id$
    Copyright (c) 2001 by Carl Eric Codere

    The base *BSD syscalls required to implement the system unit. These
    are aliased for use in other units (to avoid poluting the system units
    interface)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

{$ifdef uselibc}
  {$Linklib c}

{   var
     Errno : cint; external name 'errno';}

    function sys_time(var tloc:time_t): time_t; cdecl; external name 'time';
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function sys_close(fd : cint): cint; cdecl; external name 'close';
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function sys_unlink(const path: pchar): cint; cdecl; external name 'unlink';
    function sys_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function sys_stat(const path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function sys_chdir(const path : pchar): cint; cdecl; external name 'chdir';
    function sys_mkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function sys_rmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function sys_opendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function sys_readdir(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function sys_closedir(var dirp : dir): cint; cdecl; external name 'closedir';
    procedure sys_exit(status : cint); cdecl; external name '_exit';
    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function sys_ftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function sys_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function sys_fstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function sys_fork : pid_t; cdecl; external name 'fork';
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function sys_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';

    function sys_uname(var name: utsname): cint; cdecl; external name 'uname';

    function sys_Dup(oldd:cint):cint; cdecl; external name 'dup';
    function sys_Dup2(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';

{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{ The system designed for Linux can't be used for *BSD so easily, since
  *BSD pushes arguments, instead of loading them to registers.}

// Var ErrNo : Longint;

{$I syscall.inc}
{$I sysnr.inc}
{$I bsdmacro.inc}
{$I bsdtypes.inc}

// Should be moved to a FreeBSD specific unit in the future.

function sys_time(var tloc:time_t): time_t; [public, alias : 'FPC_SYSC_TIME'];

VAR tv     : timeval;
    tz     : timezone;
    retval : longint;

begin
  Retval:=do_syscall(syscall_nr_gettimeofday,longint(@tv),longint(@tz));
  If retval=-1 then
   sys_time:=-1
  else
   Begin
//    If Assigned(tloc) Then
     TLoc:=tv.sec;
    sys_time:=tv.sec;
   End;
End;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}

function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; [public, alias : 'FPC_SYSC_OPEN'];

Begin
 sys_open:=do_syscall(syscall_nr_open,longint(path),longint(flags),longint(mode));
End;

function sys_close(fd : cint): cint;  

begin
 sys_close:=do_syscall(syscall_nr_close,fd);
end;

function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];
{
this one is special for the return value being 64-bit..
hi/lo offset not yet tested.

NetBSD: ok, but implicit return value in edx:eax
FreeBSD: same implementation as NetBSD.
}

begin
  {ugly implicit returnvalue}
  do_syscall(syscall_nr___syscall,syscall_nr_lseek,0,longint(fd),0,lo(Offset),{0} hi(offset),Whence);
end;

function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];

begin
  sys_read:=do_syscall(syscall_nr_read,Fd,longint(buf),nbytes);
end;

function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];

begin
 sys_write:=do_syscall(syscall_nr_write,Fd,longint(buf),nbytes);
end;

function sys_unlink(const path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];

begin
  sys_unlink:=do_syscall(syscall_nr_unlink,longint(path));
end;

function sys_rename(const old : pchar; const newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];

begin
  sys_rename:=do_syscall(syscall_nr_rename,longint(old),longint(newpath));
end;

function sys_stat(const path: pchar; var buf : stat):cint; [public, alias : 'FPC_SYSC_STAT'];

begin
 sys_stat:=do_syscall(syscall_nr_stat,longint(path),longint(@buf));
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

function sys_chdir(const path : pchar): cint; [public, alias : 'FPC_SYSC_CHDIR'];

begin
 sys_chdir:=do_syscall(syscall_nr_chdir,longint(path));
end;

function sys_mkdir(const path : pchar; mode: mode_t):cint; [public, alias : 'FPC_SYSC_MKDIR'];

begin {Mode is 16-bit on F-BSD}
  sys_mkdir:=do_syscall(syscall_nr_mkdir,longint(path),mode);
end;

function sys_rmdir(const path : pchar): cint;  [public, alias : 'FPC_SYSC_RMDIR'];

begin
 sys_rmdir:=do_syscall(syscall_nr_rmdir,longint(path));
end;

{$ifndef NewReaddir}

const DIRBLKSIZ=1024;

 
function sys_opendir(const dirname : pchar): pdir;  [public, alias : 'FPC_SYSC_OPENDIR'];

var
  fd:longint;
  st:stat;
  ptr:pdir;
begin
  sys_opendir:=nil;
  if sys_stat(dirname,st)<0 then
   exit;
{ Is it a dir ? }
  if not((st.st_mode and $f000)=$4000)then
   begin
     errno:=sys_enotdir;
     exit
   end;
{ Open it}
  fd:=sys_open(dirname,O_RDONLY,438);
  if fd<0 then
   Begin
    Errno:=-1;
    exit;
   End;
  new(ptr);
  if ptr=nil then
   Begin
    Errno:=1;
    exit;
   End;
  Getmem(ptr^.dd_buf,2*DIRBLKSIZ);
  if ptr^.dd_buf=nil then
   exit;
  ptr^.dd_fd:=fd;
  ptr^.dd_loc:=-1;
  ptr^.dd_rewind:=longint(ptr^.dd_buf);
  ptr^.dd_size:=0;
//  ptr^.dd_max:=sizeof(ptr^.dd_buf^);
  sys_opendir:=ptr;
end;

function sys_closedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];

begin
  sys_closedir:=sys_close(dirp^.dd_fd);
  Freemem(dirp^.dd_buf);
  dispose(dirp);
end;

function sys_readdir(dirp : pdir) : pdirent; [public, alias : 'FPC_SYSC_READDIR'];

{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

function readbuffer:longint;

var retval :longint;

begin
 Retval:=do_syscall(syscall_nr_getdents,longint(dirp^.dd_fd),longint(@dirp^.dd_buf^),DIRBLKSIZ {sizeof(getdentsbuffer)});
   dirp^.dd_rewind:=longint(dirp^.dd_buf);
   if retval=0 then
    begin
     dirp^.dd_rewind:=0;
     dirp^.dd_loc:=0;
    end
   else
    dirP^.dd_loc:=retval;
 readbuffer:=retval;
end;

var
    FinalEntry     : pdirent;
    novalid        : boolean;
    Reclen	   : Longint;
    CurEntry	   : PDirent;

begin
 if (dirp^.dd_buf=nil) or (dirp^.dd_loc=0) THEN
  exit(nil);
 if (dirp^.dd_loc=-1)   OR     {First readdir on this pdir. Initial fill of buffer}
   (dirp^.dd_rewind>=(longint(dirp^.dd_buf)+dirblksiz)) then  {no more entries left?}
  Begin
    if readbuffer=0 then        {succesful read?}
     Exit(NIL);			{No more data}
  End;
 FinalEntry:=NIL;
 CurEntry:=nil;
 repeat
  novalid:=false;
  CurEntry:=pdirent(dirp^.dd_rewind);
  RecLen:=CurEntry^.d_reclen;
  if RecLen<>0 Then
   begin {valid direntry?}
    if CurEntry^.d_fileno<>0 then
     FinalEntry:=CurEntry;
    inc(dirp^.dd_rewind,Reclen);
   end
  else
   begin {block entirely searched or reclen=0}
    Novalid:=True;
    if dirp^.dd_loc<>0 THEN             {blocks left?}
     if readbuffer()<>0 then        {succesful read?}
      novalid:=false;
   end;
 until (FinalEntry<>nil) or novalid;
 If novalid then
  FinalEntry:=nil;
 Sys_ReadDir:=FinalEntry;
end;
{$endif}

{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

procedure sys_exit(status : cint); [public, alias : 'FPC_SYSC_EXIT'];

begin
  do_syscall(syscall_nr_exit,status);
end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; [public, alias : 'FPC_SYSC_SIGACTION'];

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
  do_syscall(syscall_nr_sigaction,longint(sig),longint(@act),longint(@oact));
end;

(*=================== MOVED from sysunix.inc ========================*)

function sys_ftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];
{ See notes lseek. This one is completely similar.

}
begin
 Do_syscall(syscall_nr___syscall,syscall_nr_ftruncate,0,fd,0,lo(flength),hi(flength));
end;

function sys_fstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

begin
  Sys_FStat:=do_SysCall(syscall_nr_fstat,fd,longint(@sb));
end;

{$ifdef NewReaddir} 
{$I readdir.inc}
{$endif}

function sys_fork : pid_t;  [public, alias : 'FPC_SYSC_FORK'];
{
  This function issues the 'fork' System call. the program is duplicated in memory
  and Execution continues in parent and child process.
  In the parent process, fork returns the PID of the child. In the child process,
  zero is returned.
  A negative value indicates that an error has occurred, the error is returned in
  LinuxError.
}

Begin
 sys_fork:=Do_syscall(SysCall_nr_fork);
End;

{
function sys_execve(const path : pathstr; const argv : ppchar; const envp: ppchar): cint; 
}
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

{
Begin
  path:=path+#0;
  do_syscall(syscall_nr_Execve,longint(@path[1]),longint(Argv),longint(envp));
End;
}

function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;  [public, alias : 'FPC_SYSC_EXECVE'];
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

Begin
  do_syscall(syscall_nr_Execve,longint(path),longint(Argv),longint(envp));
End;

function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; [public, alias : 'FPC_SYSC_WAITPID'];
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin
 sys_WaitPID:=do_syscall(syscall_nr_WaitPID,PID,longint(Stat_loc),options,0);
end;

function sys_access(const pathname : pchar; amode : cint): cint; [public, alias : 'FPC_SYSC_ACCESS'];
{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in unixerror.
}

begin
 sys_Access:=do_syscall(syscall_nr_access,longint(pathname),amode);
end;
{
function sys_access(const pathname : pathstr; amode : cint): cint;

{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in unixerror.
}

begin
 pathname:=pathname+#0;
 Access:=do_syscall(syscall_nr_access, longint(@pathname[1]),mode)=0;
end;
}

function sys_Dup(oldd:cint):cint; [public, alias : 'FPC_SYSC_DUP'];
{
  Copies the filedescriptor oldfile to newfile
}

begin
  sys_dup:=Do_syscall(syscall_nr_dup,oldd);
end;

function sys_Dup2(oldd:cint;newd:cint):cint; [public, alias : 'FPC_SYSC_DUP2'];
{
  Copies the filedescriptor oldfile to newfile
}

begin
 sys_dup2:=do_syscall(syscall_nr_dup2,oldd,newd);
end;

CONST
 { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$1000;


Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint;  [public, alias : 'FPC_SYSC_MMAP'];
begin
  Sys_mmap:=do_syscall(syscall_nr_mmap,Adr,Len,Prot,Flags,fdes,off,0);
end;

Function sbrk(size : longint) : Longint;
begin
  sbrk:=Sys_mmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if sbrk<>-1 then
   errno:=0;
  {! It must be -1, not 0 as before, see heap.inc. Should be in sysmmap?}
end;

{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
Function Sys_IOCtl(Handle,Ndx: Longint;Data: Pointer):LongInt;  [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  Sys_IOCtl:=do_SysCall(syscall_nr_ioctl,handle,Ndx,longint(data));
end;

CONST
  IOCtl_TCGETS=$5401;

Function Do_IsDevice(Handle:Longint):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
var
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  Do_IsDevice:=(sys_ioctl(handle,IOCTL_TCGETS,@data)<>-1);
end;

Function sys_GetPid:LongInt;   [public, alias : 'FPC_SYSC_GETPID'];
{
  Get Process ID.
}

begin
 sys_GetPID:=do_syscall(syscall_nr_getpid);
end;

Function Sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;  [public, alias : 'FPC_SYSC_READLINK'];

begin
  sys_readlink:=do_syscall(syscall_nr_readlink, longint(name),longint(linkname),maxlen);
end;



{$endif}

{
 $Log$
 Revision 1.2  2002-08-21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.1  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.


 Revision 1.2  2002/08/04 04:29:34  marco
  * More POSIX updates. Small changes to lseek and ftruncate in osposix.inc
    Initial versions of the type includefiles

 Revision 1.1  2002/08/03 19:34:19  marco
  * Initial *BSD versions. Seems that OpenBSD doesn't need much change,
     NetBSD may need some fixes to stat record and ftruncate and lseek.
     It is all close together, and it should be doable to have just one copy
     of these for *BSD.

 Revision 1.1.2.5  2001/12/09 03:31:50  carl
 + wifsignaled() added

 Revision 1.1.2.4  2001/12/03 03:13:30  carl
 * fix ftruncate prototype
 * fix rename prototype
 * change readdir / closedir prototype

 Revision 1.1.2.3  2001/11/30 03:50:43  carl
 + int -> cint
 + missing prototypes added

 Revision 1.1.2.2  2001/11/28 03:08:29  carl
 * int -> cint
 + several other stuff renamed

 Revision 1.1.2.1  2001/08/15 00:15:04  carl
 - renamed

}