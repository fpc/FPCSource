{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    The base *BSD syscalls required to implement the system unit. These
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

    function Fptime(tloc:ptime_t): time_t; cdecl; external name 'time';
    function Fpopen(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function Fpclose(fd : cint): cint; cdecl; external name 'close';
    function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function Fpwrite(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function Fpunlink(const path: pchar): cint; cdecl; external name 'unlink';
    function Fprename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function Fpstat(const path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function Fpchdir(const path : pchar): cint; cdecl; external name 'chdir';
    function Fpmkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function Fprmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function Fpopendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function Fpreaddir(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function Fpclosedir(var dirp : dir): cint; cdecl; external name 'closedir';
    procedure Fpexit(status : cint); cdecl; external name '_exit';
    function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function Fpftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function Fprename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function Fpfstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function Fpfork : pid_t; cdecl; external name 'fork';
    function Fpexecve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function Fpwaitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function Fpaccess(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';

    function Fpuname(var name: utsname): cint; cdecl; external name 'uname';

    function FpDup(oldd:cint):cint; cdecl; external name 'dup';
    function FpDup2(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';

{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{ The system designed for Linux can't be used for *BSD so easily, since
  *BSD pushes arguments, instead of loading them to registers.}

// Var ErrNo : Longint;

{$I syscallh.inc}
{$I syscall.inc}
{$I sysnr.inc}
{$I bunxmacr.inc}
{$I ostypes.inc}

// Should be moved to a FreeBSD specific unit in the future.

function Fptime( tloc:ptime): time_t; [public, alias : 'FPC_SYSC_TIME'];

VAR tv     : timeval;
    tz     : timezone;
    retval : longint;

begin
  Retval:=do_syscall(syscall_nr_gettimeofday,TSysParam(@tv),TSysParam(@tz));
  If retval=-1 then
   Fptime:=-1
  else
   Begin
   If Assigned(tloc) Then
     TLoc^:=tv.tv_sec;
    Fptime:=tv.tv_sec;
   End;
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

{$ifdef netbsd}
  {$ifdef cpupowerpc}
    {$define netbsdmacppc}
  {$endif}
{$endif}

{$ifdef netbsdmacppc}
{$i sysofft.inc}			// odd ball calling convention.
{$else}
  // generic versions.
function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];

{
this one is special for the return value being 64-bit..
hi/lo offset not yet tested.

NetBSD: ok, but implicit return value in edx:eax
FreeBSD: same implementation as NetBSD.
}

begin
  Fplseek:=do_syscall(syscall_nr___syscall,syscall_nr_lseek,0,TSysParam(fd),0,lo(Offset),{0} hi(offset),Whence);
end;

function Fpftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];

begin
 Fpftruncate:=Do_syscall(syscall_nr___syscall,syscall_nr_ftruncate,0,fd,0,lo(flength),hi(flength));
end;

Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; [public, alias:  'FPC_SYSC_MMAP'];

begin
  Fpmmap:=pointer(longint(do_syscall(syscall_nr_mmap,TSysParam(Start),Len,Prot,Flags,fd,{$ifdef cpupowerpc}0,{$endif}offst{$ifdef i386},0{$endif})));
end;

{$endif}


function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];

begin
  Fpread:=do_syscall(syscall_nr_read,Fd,TSysParam(buf),nbytes);
end;

function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];

begin
 Fpwrite:=do_syscall(syscall_nr_write,Fd,TSysParam(buf),nbytes);
end;

function Fpunlink(const path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];

begin
  Fpunlink:=do_syscall(syscall_nr_unlink,TSysParam(path));
end;

function Fprename(old : pchar; newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];

begin
  Fprename:=do_syscall(syscall_nr_rename,TSysParam(old),TSysParam(newpath));
end;

function Fpstat(const path: pchar; var buf : stat):cint; [public, alias : 'FPC_SYSC_STAT'];

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

begin {Mode is 16-bit on F-BSD 4!}
  Fpmkdir:=do_syscall(syscall_nr_mkdir,TSysParam(path),mode);
end;

function Fprmdir(path : pchar): cint;  [public, alias : 'FPC_SYSC_RMDIR'];

begin
 Fprmdir:=do_syscall(syscall_nr_rmdir,TSysParam(path));
end;

{$ifndef NewReaddir}

const DIRBLKSIZ=1024;


function Fpopendir(dirname : pchar): pdir;  [public, alias : 'FPC_SYSC_OPENDIR'];

var
  fd:longint;
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
  Fpopendir:=ptr;
end;

function Fpclosedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];

begin
  Fpclosedir:=Fpclose(dirp^.dd_fd);
  Freemem(dirp^.dd_buf);
  dispose(dirp);
end;

function Fpreaddir(dirp : pdir) : pdirent; [public, alias : 'FPC_SYSC_READDIR'];

{Different from Linux, Readdir on BSD is based on Getdents, due to the
missing of the readdir syscall.
Getdents requires the buffer to be larger than the blocksize.
This usually the sectorsize =512 bytes, but maybe tapedrives and harddisks
with blockmode have this higher?}

function readbuffer:longint;

var retval :longint;

begin
 Retval:=do_syscall(syscall_nr_getdents,TSysParam(dirp^.dd_fd),TSysParam(@dirp^.dd_buf^),DIRBLKSIZ {sizeof(getdentsbuffer)});
   dirp^.dd_rewind:=TSysParam(dirp^.dd_buf);
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
    Reclen         : Longint;
    CurEntry       : PDirent;

begin
 if (dirp^.dd_buf=nil) or (dirp^.dd_loc=0) THEN
  exit(nil);
 if (dirp^.dd_loc=-1)   OR     {First readdir on this pdir. Initial fill of buffer}
   (dirp^.dd_rewind>=(longint(dirp^.dd_buf)+dirblksiz)) then  {no more entries left?}
  Begin
    if readbuffer=0 then        {succesful read?}
     Exit(NIL);                 {No more data}
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
 FpReadDir:=FinalEntry;
end;
{$endif}

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

function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; [public, alias : 'FPC_SYSC_SIGACTION'];

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
  do_syscall(syscall_nr_sigaction,TSysParam(sig),TSysParam(@act),TSysParam(@oact));
end;

(*=================== MOVED from sysunix.inc ========================*)


function Fpfstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

begin
  fpFStat:=do_SysCall(syscall_nr_fstat,fd,TSysParam(@sb));
end;

{$ifdef NewReaddir}
{$I readdir.inc}
{$endif}

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

{
function Fpexecve(const path : pathstr; const argv : ppchar; const envp: ppchar): cint;
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
function Fpexecve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;  [public, alias : 'FPC_SYSC_EXECVE'];
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
function Fpwaitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; [public, alias : 'FPC_SYSC_WAITPID'];
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
 FpWaitPID:=do_syscall(syscall_nr_WaitPID,PID,TSysParam(@Stat_loc),options,0);
end;

function Fpaccess(const pathname : pchar; amode : cint): cint; [public, alias : 'FPC_SYSC_ACCESS'];
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
{
function Fpaccess(const pathname : pathstr; amode : cint): cint;

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
  MAP_ANONYMOUS =$1000;


Function Fpmunmap(start:pointer;len:size_t):cint;    [public, alias :'FPC_SYSC_MUNMAP'];
begin
  Fpmunmap:=do_syscall(syscall_nr_munmap,TSysParam(start),Len);
end;

Function sbrk(size : longint) : cint;
begin
  sbrk:=longint(Fpmmap(0,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0)); // ugh
  if sbrk<>(-1) then
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

Function FpIOCtl(Handle:cint;Ndx: culong;Data: Pointer):cint; [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  FpIOCtl:=do_SysCall(syscall_nr_ioctl,handle,Ndx,TSysParam(data));
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
  Do_IsDevice:=(Fpioctl(handle,IOCTL_TCGETS,@data)<>-1);
end;

Function FpGetPid:LongInt;   [public, alias : 'FPC_SYSC_GETPID'];
{
  Get Process ID.
}

begin
 FpGetPID:=do_syscall(syscall_nr_getpid);
end;

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; [public, alias: 'FPC_SYSC_GETTIMEOFDAY'];

begin
 fpgettimeofday:=do_syscall(syscall_nr_gettimeofday,TSysParam(tp),TSysParam(tzp));
end;

Function FpNanoSleep(const req : timespec;var rem : timespec) : longint; [public, alias : 'FPC_SYSC_NANOSLEEP'];
begin
{$ifndef darwin}
  FpNanoSleep:=Do_SysCall(syscall_nr_nanosleep,TSysParam(@req),TSysParam(@rem));
{$else not darwin}
{$warning: TODO: nanosleep!!!}
{$endif not darwin}
end;

function Fpgetcwd(pt:pchar; _size:size_t):pchar;[public, alias :'FPC_SYSC_GETCWD'];
{$ifndef darwin}
const intpathmax = 1024-4;	// didn't use POSIX data in libc
				// implementation.
var ept,bpt : pchar;
    c	    : char;
    ret	    : cint;

begin
   if pt=NIL Then
    begin 
      // POSIX: undefined. (exit(nil) ?)
      // BSD  : allocate mem for path.
      getmem(pt,intpathmax); 
      if pt=nil Then 
        exit(nil);
      ept:=pt+intpathmax;
    end 
   else
    Begin
      if (_size=0) Then
        Begin
          seterrno(ESysEINVAL);
	  exit(nil);
        End; 
      if (_size=1) Then
        Begin
          seterrno(ESysERANGE);
	  exit(nil);
        End; 
      ept:=pt+_size;
    end; 

    ret := do_syscall(syscall_nr___getcwd,TSysParam(pt),TSysParam( ept - pt));
    If (ret = 0) Then 
	If (pt[0] <> '/') Then
	   Begin
	     bpt := pt;
	     ept := pt + strlen(pt) - 1;
	     While (bpt < ept) Do
	       Begin
  		 c := bpt^;
 		 bpt^:=ept^;
		 inc(bpt);
		 ept^:=c;
		 dec(ept);		
               End;
           End;
 Fpgetcwd:=pt;
end;
{$else not darwin}
{$i getcwd.inc}
{$endif darwin}

{$endif}

{
 $Log$
 Revision 1.8  2003-09-16 12:45:49  marco
  * mmap typing fixes

 Revision 1.7  2003/09/15 20:08:49  marco
  * small fixes. FreeBSD now cycles

 Revision 1.6  2003/09/14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.5  2003/08/21 22:23:34  olle
   - removed parameter from fpc_iocheck

 Revision 1.4  2003/05/29 21:45:23  marco
  * Some other workaround

 Revision 1.3  2003/05/29 20:52:55  marco
  * only moved around the off_t calls, and made an exception (includefile)
 for NetBSD/powerpc

 Revision 1.2  2003/05/26 21:29:16  jonas
   - disabled nanosleep for darwin for now
   + getcwd for darwin

 Revision 1.1  2003/01/05 19:01:28  marco
  * FreeBSD compiles now with baseunix mods.

 Revision 1.9  2002/11/13 18:15:08  marco
  * sigset functions more flexible, small changes to sys_ktime

 Revision 1.8  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.7  2002/10/27 11:58:29  marco
  * Modifications from Saturday.

 Revision 1.6  2002/10/26 18:27:51  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.5  2002/10/18 12:19:58  marco
  * Fixes to get the generic *BSD RTL compiling again + fixes for thread
    support. Still problems left in fexpand. (inoutres?) Therefore fixed
    sysposix not yet commited

 Revision 1.4  2002/10/16 18:44:18  marco
  * Lseek and ftruncate 64-bit fix

 Revision 1.3  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.2  2002/08/21 07:03:16  marco
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

}
