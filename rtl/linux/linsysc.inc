{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    The base Linux syscalls required to implement the system unit. These
    are aliased for use in other units (to avoid poluting the system units
    interface)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

{$ifdef uselibc}
  {$Linklib c}
  // Out of date atm.

{   var
     Errno : cint; external name 'errno';}

    function sys_time(tloc:ptime_t): time_t; cdecl; external name 'time';
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
    function sys_waitpid(pid : pid_t; stat_loc : pcint; options: cint): pid_t; cdecl; external name 'waitpid';
    function sys_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';

    function sys_uname(var name: utsname): cint; cdecl; external name 'uname';

    function sys_Dup(oldd:cint):cint; cdecl; external name 'dup';
    function sys_Dup2(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';

{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{$I ostypes.inc}

{$I syscall.inc}
{$I sysnr.inc}
{$I posmacro.inc}

function sys_time(tloc:ptime_t): time_t; [public, alias : 'FPC_SYSC_TIME'];

begin
  sys_time:=do_syscall(syscall_nr_time,TSysParam(tloc));
End;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}

function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; [public, alias : 'FPC_SYSC_OPEN'];

Begin
 sys_open:=do_syscall(syscall_nr_open,TSysParam(path),TSysParam(flags),TSysParam(mode));
End;

function sys_close(fd : cint): cint;

begin
 sys_close:=do_syscall(syscall_nr_close,fd);
end;

function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];
{
Must be adapted/overloaded for 64-bit support, but that is a different call under
Linux?
}
begin
  sys_lseek:=do_syscall(syscall_nr_lseek,tsysparam(fd),tsysparam(offset),tsysparam(whence));
end;

function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];

begin
  sys_read:=do_syscall(syscall_nr_read,Fd,TSysParam(buf),nbytes);
end;

function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];

begin
 sys_write:=do_syscall(syscall_nr_write,Fd,TSysParam(buf),nbytes);
end;

function sys_unlink(const path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];

begin
  sys_unlink:=do_syscall(syscall_nr_unlink,TSysParam(path));
end;

function sys_rename(const old : pchar; const newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];

begin
  sys_rename:=do_syscall(syscall_nr_rename,TSysParam(old),TSysParam(newpath));
end;

function sys_stat(const path: pchar; var buf : stat):cint; [public, alias : 'FPC_SYSC_STAT'];

begin
 sys_stat:=do_syscall(syscall_nr_stat,TSysParam(path),TSysParam(@buf));
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

function sys_chdir(const path : pchar): cint; [public, alias : 'FPC_SYSC_CHDIR'];

begin
 sys_chdir:=do_syscall(syscall_nr_chdir,TSysParam(path));
end;

function sys_mkdir(const path : pchar; mode: mode_t):cint; [public, alias : 'FPC_SYSC_MKDIR'];

begin 
  sys_mkdir:=do_syscall(syscall_nr_mkdir,TSysParam(path),TSysParam(mode));
end;

function sys_rmdir(const path : pchar): cint;  [public, alias : 'FPC_SYSC_RMDIR'];

begin
 sys_rmdir:=do_syscall(syscall_nr_rmdir,TSysParam(path));
end;

function sys_opendir(const dirname : pchar): pdir;  [public, alias : 'FPC_SYSC_OPENDIR'];

var
  fd:integer;
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
   exit;
  new(ptr);
  if ptr=nil then
   exit;
  getmem(ptr^.buf,sizeof(dirent));
  if ptr^.buf=nil then
   exit;
  ptr^.fd:=fd;
  ptr^.loc:=0;
  ptr^.size:=0;
  ptr^.dd_max:=sizeof(ptr^.buf^);
  sys_opendir:=ptr;
end;

function sys_closedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];

begin
  sys_closedir:=sys_close(dirp^.fd);
  freemem(dirp^.buf,sizeof(dirent));
  dispose(dirp);
end;

function sys_readdir(dirp : pdir) : pdirent; [public, alias : 'FPC_SYSC_READDIR'];

{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

begin
  if do_SysCall(SysCall_nr_readdir,TSysParam(dirp^.fd),TSysParam(dirp^.buf),TSysParam(1))=0 Then
{ the readdir system call returns the number of bytes written }
   sys_readdir:=nil
  else
   sys_readdir:=dirp^.buf
end;

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
  do_syscall(syscall_nr_sigaction,TSysParam(sig),TSysParam(@act),TSysParam(@oact));
end;

function sys_ftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];
{ See notes lseek. This one is completely similar for the parameter (but
doesn't have the returnvalue 64-bit problem)}

begin
 sys_ftruncate:=Do_syscall(syscall_nr_ftruncate,TSysParam(fd),TSysParam(flength));
end;

function sys_fstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

begin
  Sys_FStat:=do_SysCall(syscall_nr_fstat,TSysParam(fd),TSysParam(@sb));
end;


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

// Look at execve variants later, when overloaded is determined.
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
  do_syscall(syscall_nr_Execve,TSysParam(@path[1]),TSysParam(Argv),TSysParam(envp));
End;
}
{
function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;  [public, alias : 'FPC_SYSC_EXECVE'];
}
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}
{
Begin
  do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(Argv),TSysParam(envp));
End;
}

function sys_waitpid(pid : pid_t; stat_loc : pcint; options: cint): pid_t; [public, alias : 'FPC_SYSC_WAITPID'];
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin
 sys_WaitPID:=do_syscall(syscall_nr_WaitPID,PID,TSysParam(Stat_loc),options);
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
 sys_Access:=do_syscall(syscall_nr_access,TSysParam(pathname),amode);
end;

{ overloaded
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
 Access:=do_syscall(syscall_nr_access, TSysParam(@pathname[1]),mode)=0;
end;
}

Function sys_Dup(fildes:cint):cint; [public, alias : 'FPC_SYSC_DUP'];

begin
  sys_dup:=Do_syscall(syscall_nr_dup,TSysParam(fildes));
end;

Function sys_Dup2(fildes,fildes2:cint):cint; [public, alias : 'FPC_SYSC_DUP2'];

begin
 sys_dup2:=do_syscall(syscall_nr_dup2,TSysParam(fildes),TSysParam(fildes2));
end;

CONST

  { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$20;

  {Constansts Termios/Ioctl (used in Do_IsDevice) }
  IOCtl_TCGETS=$5401; // TCGETS is also in termios.inc, but the sysunix needs only this

type
  tmmapargs=packed record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;


Function Sys_mmap(adr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;off:off_t):pointer;  [public, alias : 'FPC_SYSC_MMAP'];
// OFF_T procedure, and returns a pointer, NOT cint.

var
  mmapargs : tmmapargs;
begin
  mmapargs.address:=TSysParam(adr);
  mmapargs.size:=TSysParam(len);
  mmapargs.prot:=TSysParam(prot);
  mmapargs.flags:=TSysParam(flags);
  mmapargs.fd:=TSysParam(fd);
  mmapargs.offset:=TSysParam(off);
  Sys_mmap:=pointer(do_syscall(syscall_nr_mmap,TSysParam(@MMapArgs)));
end;

Function Sys_munmap(adr:pointer;len:size_t):cint; [public, alias :'FPC_SYSC_MUNMAP'];
begin
  Sys_munmap:=do_syscall(syscall_nr_munmap,TSysParam(Adr),TSysParam(Len));
end;

Function sbrk(size : longint) : longint;
begin
  sbrk:=longint(Sys_mmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
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

// prototype is cint __P(cint,culong,....)
// actual meaning of return value depends on request.

Function Sys_IOCtl(fd:cint;request:culong;Data: Pointer):cint;  [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  Sys_IOCtl:=do_SysCall(syscall_nr_ioctl,tsysparam(fd),tsysparam(Request),TSysParam(data));
end;

Function Do_IsDevice(Handle:cint):boolean;
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

Function sys_GetPid:pid_t;   [public, alias : 'FPC_SYSC_GETPID'];
{
  Get Process ID.
}

begin
 sys_GetPID:=do_syscall(syscall_nr_getpid);
end;

Function Sys_ReadLink(name,linkname:pchar;maxlen:size_t):cint;  [public, alias : 'FPC_SYSC_READLINK'];

begin
  sys_readlink:=do_syscall(syscall_nr_readlink, TSysParam(name),TSysParam(linkname),maxlen);
end;

Function sys_NanoSleep(const req : timespec;rem : ptimespec) : longint; [public, alias : 'FPC_SYSC_NANOSLEEP'];
begin
  sys_NanoSleep:=Do_SysCall(syscall_nr_nanosleep,TSysParam(@req),TSysParam(rem));
end;

// The following belongs here, but this should be researched more.
// function sys_getcwd(pt:pchar; _size:size_t):pchar;[public, alias :'FPC_SYSC_GETCWD'];

{$endif}

{
 $Log$
 Revision 1.1  2002-11-12 14:40:18  marco
  * The syscall core of the new system unit.


}
