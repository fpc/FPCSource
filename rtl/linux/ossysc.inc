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
{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{$I ostypes.inc}
{$I syscallh.inc}
{$I syscall.inc}
{$I sysnr.inc}
{$I bunxmacr.inc}

function Fptime(tloc:pTime): TTime; [public, alias : 'FPC_SYSC_TIME'];

begin
  Fptime:=do_syscall(syscall_nr_time,TSysParam(tloc));
End;

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}

function Fpopen(path: pchar; flags : cint; mode: mode_t):cint; [public, alias : 'FPC_SYSC_OPEN'];

Begin
 Fpopen:=do_syscall(syscall_nr_open,TSysParam(path),TSysParam(flags),TSysParam(mode));
End;

function Fpclose(fd : cint): cint; [public, alias : 'FPC_SYSC_CLOSE'];

begin
 Fpclose:=do_syscall(syscall_nr_close,fd);
end;

function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];
{
Must be adapted/overloaded for 64-bit support, but that is a different call under
Linux?
}
begin
  Fplseek:=do_syscall(syscall_nr_lseek,tsysparam(fd),tsysparam(offset),tsysparam(whence));
end;

function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];

begin
  Fpread:=do_syscall(syscall_nr_read,Fd,TSysParam(buf),nbytes);
end;

function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];

begin
 Fpwrite:=do_syscall(syscall_nr_write,Fd,TSysParam(buf),nbytes);
end;

function Fpunlink(path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];

begin
  Fpunlink:=do_syscall(syscall_nr_unlink,TSysParam(path));
end;

function Fprename(old : pchar; newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];

begin
  Fprename:=do_syscall(syscall_nr_rename,TSysParam(old),TSysParam(newpath));
end;

function Fpstat(path: pchar; var buf : stat):cint; [public, alias : 'FPC_SYSC_STAT'];

begin
 Fpstat:=do_syscall(syscall_nr_stat,TSysParam(path),TSysParam(@buf));
end;


{*****************************************************************************
               --- Directory:Directory related calls ---
*****************************************************************************}

function Fpchdir(path : pchar): cint; [public, alias : 'FPC_SYSC_CHDIR'];

begin
 Fpchdir:=do_syscall(syscall_nr_chdir,TSysParam(path));
end;

function Fpmkdir(path : pchar; mode: mode_t):cint; [public, alias : 'FPC_SYSC_MKDIR'];

begin 
  Fpmkdir:=do_syscall(syscall_nr_mkdir,TSysParam(path),TSysParam(mode));
end;

function Fprmdir(path : pchar): cint;  [public, alias : 'FPC_SYSC_RMDIR'];

begin
 Fprmdir:=do_syscall(syscall_nr_rmdir,TSysParam(path));
end;

function Fpopendir(dirname : pchar): pdir;  [public, alias : 'FPC_SYSC_OPENDIR'];

var
  fd:integer;
  st:stat;
  ptr:pdir;

begin
  Fpopendir:=nil;
  if Fpstat(dirname,st)<0 then
   exit;
{ Is it a dir ? }
  if not((st.st_mode and $f000)=$4000)then
   begin
     errno:=ESysENOTDIR;
     exit
   end;
{ Open it}
  fd:=Fpopen(dirname,O_RDONLY,438);
  if fd<0 then
   exit;
  new(ptr);
  if ptr=nil then
   exit;
  getmem(ptr^.dd_buf,sizeof(dirent));
  if ptr^.dd_buf=nil then
   exit;
  ptr^.dd_fd:=fd;
  ptr^.dd_loc:=0;
  ptr^.dd_size:=0;
  ptr^.dd_max:=sizeof(ptr^.dd_buf^);
  Fpopendir:=ptr;
end;

function Fpclosedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];

begin
  Fpclosedir:=Fpclose(dirp^.dd_fd);
  freemem(dirp^.dd_buf,sizeof(dirent));
  dispose(dirp);
end;

function Fpreaddir(dirp : pdir) : pdirent; [public, alias : 'FPC_SYSC_READDIR'];

{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

begin
  if do_SysCall(SysCall_nr_readdir,TSysParam(dirp^.dd_fd),TSysParam(dirp^.dd_buf),TSysParam(1))=0 Then
{ the readdir system call returns the number of bytes written }
   Fpreaddir:=nil
  else
   Fpreaddir:=dirp^.dd_buf
end;

{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

procedure Fpexit(status : cint); [public, alias : 'FPC_SYSC_EXIT'];

begin
  do_syscall(syscall_nr_exit,status);
end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

function Fpsigaction(sig: cint; act : psigactionrec; oact : psigactionrec): cint; [public, alias : 'FPC_SYSC_SIGACTION'];

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
  do_syscall(syscall_nr_sigaction,TSysParam(sig),TSysParam(act),TSysParam(oact));
end;

function Fpftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];
{ See notes lseek. This one is completely similar for the parameter (but
doesn't have the returnvalue 64-bit problem)}

begin
 Fpftruncate:=Do_syscall(syscall_nr_ftruncate,TSysParam(fd),TSysParam(flength));
end;

function Fpfstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

begin
  FpFStat:=do_SysCall(syscall_nr_fstat,TSysParam(fd),TSysParam(@sb));
end;


function Fpfork : pid_t;  [public, alias : 'FPC_SYSC_FORK'];
{
  This function issues the 'fork' System call. the program is duplicated in memory
  and Execution continues in parent and child process.
  In the parent process, fork returns the PID of the child. In the child process,
  zero is returned.
  A negative value indicates that an error has occurred, the error is returned in
  LinuxError.
}

Begin
 Fpfork:=Do_syscall(SysCall_nr_fork);
End;

// Look at execve variants later, when overloaded is determined.
{
function Fpexecve(path : pathstr; argv : ppchar; envp: ppchar): cint;
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
function Fpexecve(path : pchar; argv : ppchar; envp: ppchar): cint;  [public, alias : 'FPC_SYSC_EXECVE'];
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

function Fpwaitpid(pid : pid_t; stat_loc : pcint; options: cint): pid_t; [public, alias : 'FPC_SYSC_WAITPID'];
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin
 FpWaitPID:=do_syscall(syscall_nr_WaitPID,PID,TSysParam(Stat_loc),options);
end;

function Fpaccess(pathname : pchar; amode : cint): cint; [public, alias : 'FPC_SYSC_ACCESS'];
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
 FpAccess:=do_syscall(syscall_nr_access,TSysParam(pathname),amode);
end;

{ overloaded
function Fpaccess(pathname : pathstr; amode : cint): cint;

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

Function FpDup(fildes:cint):cint; [public, alias : 'FPC_SYSC_DUP'];

begin
  Fpdup:=Do_syscall(syscall_nr_dup,TSysParam(fildes));
end;

Function FpDup2(fildes,fildes2:cint):cint; [public, alias : 'FPC_SYSC_DUP2'];

begin
 Fpdup2:=do_syscall(syscall_nr_dup2,TSysParam(fildes),TSysParam(fildes2));
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


{$ifdef i386}
{$define OLDMMAP}
{$endif i386}

{$ifdef m68k}
{$define OLDMMAP}
{$endif m68k}

Function Fpmmap(adr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;off:off_t):pointer;  [public, alias : 'FPC_SYSC_MMAP'];
// OFF_T procedure, and returns a pointer, NOT cint.

{$ifdef OLDMMAP}
var
  mmapargs : tmmapargs;
begin
  mmapargs.address:=TSysParam(adr);
  mmapargs.size:=TSysParam(len);
  mmapargs.prot:=TSysParam(prot);
  mmapargs.flags:=TSysParam(flags);
  mmapargs.fd:=TSysParam(fd);
  mmapargs.offset:=TSysParam(off);
  Fpmmap:=pointer(do_syscall(syscall_nr_mmap,TSysParam(@MMapArgs)));
end;
{$else OLDMMAP}
begin
  Fpmmap:= pointer(do_syscall(syscall_nr_mmap,TSysParam(adr),TSysParam(len),
    TSysParam(prot),TSysParam(flags),TSysParam(fd),TSysParam(off)));
end;
{$endif OLDMMAP}


Function Fpmunmap(adr:pointer;len:size_t):cint; [public, alias :'FPC_SYSC_MUNMAP'];
begin
  Fpmunmap:=do_syscall(syscall_nr_munmap,TSysParam(Adr),TSysParam(Len));
end;

Function sbrk(size : longint) : pointer;
begin
  sbrk:=Fpmmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if sbrk=pointer(-1) then
    sbrk:=nil
  else 
    errno:=0;
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

Function FpIOCtl(fd:cint;request:culong;Data: Pointer):cint;  [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  FpIOCtl:=do_SysCall(syscall_nr_ioctl,tsysparam(fd),tsysparam(Request),TSysParam(data));
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
  Do_IsDevice:=(Fpioctl(handle,IOCTL_TCGETS,@data)<>-1);
end;

Function FpGetPid:pid_t;   [public, alias : 'FPC_SYSC_GETPID'];
{
  Get Process ID.
}

begin
 FpGetPID:=do_syscall(syscall_nr_getpid);
end;

Function FpReadLink(name,linkname:pchar;maxlen:size_t):cint;  [public, alias : 'FPC_SYSC_READLINK'];

begin
  Fpreadlink:=do_syscall(syscall_nr_readlink, TSysParam(name),TSysParam(linkname),maxlen);
end;

Function FpNanoSleep(const req : timespec;rem : ptimespec) : longint; [public, alias : 'FPC_SYSC_NANOSLEEP'];
begin
  FpNanoSleep:=Do_SysCall(syscall_nr_nanosleep,TSysParam(@req),TSysParam(rem));
end;

// The following belongs here, but this should be researched more.
// function Fpgetcwd(pt:pchar; _size:size_t):pchar;[public, alias :'FPC_SYSC_GETCWD'];

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; [public, alias:
'FPC_SYSC_GETTIMEOFDAY'];

begin
 fpgettimeofday:=do_syscall(syscall_nr_gettimeofday,TSysParam(tp),TSysParam(tzp));
end;

{$ENDIF}

{
 $Log$
 Revision 1.7  2003-09-27 12:58:23  peter
   * mmap returns -1 on error

 Revision 1.6  2003/09/27 11:52:35  peter
   * sbrk returns pointer

 Revision 1.5  2003/09/15 20:29:50  marco
  * small fix

 Revision 1.4  2003/09/14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.3  2003/05/11 16:07:55  jonas
   * fixed mmap for non-i386 non-m68k architectures (not sure about
     x86-64)

 Revision 1.2  2002/12/18 17:38:01  marco
  * small fix, new rtl now cycles

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.1  2002/11/12 14:40:18  marco
  * The syscall core of the new system unit.


}
