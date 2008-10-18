{
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

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

function Fptime(tloc:pTime): TTime; [public, alias : 'FPC_SYSC_TIME'];
{$ifdef FPC_USEGETTIMEOFDAY}
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
{$else FPC_USEGETTIMEOFDAY}
begin
  Fptime:=do_syscall(syscall_nr_time,TSysParam(tloc));
end;
{$endif FPC_USEGETTIMEOFDAY}

{*****************************************************************************
               --- File:File handling related calls ---
*****************************************************************************}

function Fpopen(path: pchar; flags : cint; mode: mode_t):cint; [public, alias : 'FPC_SYSC_OPEN'];

Begin
 Fpopen:=do_syscall(syscall_nr_open,TSysParam(path),TSysParam(flags or O_LARGEFILE),TSysParam(mode));
End;

function Fpclose(fd : cint): cint; [public, alias : 'FPC_SYSC_CLOSE'];

begin
 Fpclose:=do_syscall(syscall_nr_close,fd);
end;

function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; [public, alias : 'FPC_SYSC_LSEEK'];
begin
{$if defined(cpu64)}
  result:=do_syscall(syscall_nr_lseek,tsysparam(fd),tsysparam(offset),tsysparam(whence));
{$else}
  if do_syscall(syscall_nr__llseek,tsysparam(fd),
//      {$ifdef FPC_ABI_EABI}      0,  { align parameters as required with dummy } {$endif FPC_ABI_EABI}
      tsysparam(hi(offset)),tsysparam(lo(offset)),
      tsysparam(@result), tsysparam(whence)) = -1 then
    result:=off_t(-1);
{$endif}
end;

function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_READ'];

begin
  Fpread:=do_syscall(syscall_nr_read,Fd,TSysParam(buf),nbytes);
end;

function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t; [public, alias : 'FPC_SYSC_WRITE'];

begin
 Fpwrite:=do_syscall(syscall_nr_write,Fd,TSysParam(buf),nbytes);
end;
{
function Fppread(fd: cint; buf: pchar; nbytes : size_t; offset:Toff): ssize_t; [public, alias : 'FPC_SYSC_PREAD'];
   !! check 64 bit off_t sycall
begin
  Fpread:=do_syscall(syscall_nr_pread,Fd,TSysParam(buf),nbytes,offset);
end;

function Fppwrite(fd: cint;buf:pchar; nbytes : size_t; offset:Toff): ssize_t; [public, alias : 'FPC_SYSC_PWRITE'];
   !! check 64 bit off_t sycall
begin
 Fpwrite:=do_syscall(syscall_nr_pwrite,Fd,TSysParam(buf),nbytes,offset);
end;
}
function Fpunlink(path: pchar): cint; [public, alias : 'FPC_SYSC_UNLINK'];

begin
  Fpunlink:=do_syscall(syscall_nr_unlink,TSysParam(path));
end;

function Fprename(old : pchar; newpath: pchar): cint; [public, alias : 'FPC_SYSC_RENAME'];

begin
  Fprename:=do_syscall(syscall_nr_rename,TSysParam(old),TSysParam(newpath));
end;

function Fpstat(path: pchar; var buf: stat):cint; [public, alias : 'FPC_SYSC_STAT'];

begin
{$if defined(cpu64)}
  Fpstat:=do_syscall(syscall_nr_stat,TSysParam(path),TSysParam(@buf));
{$else}
  Fpstat:=do_syscall(syscall_nr_stat64,TSysParam(path),TSysParam(@buf));
{$endif}
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
  new(ptr^.dd_buf);
  if ptr^.dd_buf=nil then
   exit;
  ptr^.dd_fd:=fd;
  ptr^.dd_loc:=0;
  ptr^.dd_size:=0;
  ptr^.dd_nextoff:=0;
  ptr^.dd_max:=sizeof(ptr^.dd_buf^);
  Fpopendir:=ptr;
end;

function Fpclosedir(dirp : pdir): cint; [public, alias : 'FPC_SYSC_CLOSEDIR'];

begin
  Fpclosedir:=Fpclose(dirp^.dd_fd);
  dispose(dirp^.dd_buf);
  dispose(dirp);
end;

Function Fpreaddir(dirp : pdir) : pdirent; [public, alias: 'FPC_SYSC_READDIR'];
var bytes : longint;
    dp    : pdirent;
begin
  repeat
    if dirp^.dd_nextoff >= dirp^.dd_size then
      begin
        bytes := do_SysCall(SysCall_nr_getdents64,TSysParam(dirp^.dd_fd),TSysParam(dirp^.dd_buf),TSysParam(dirp^.dd_max));
        if bytes <= 0 then
          begin
            fpreaddir := nil;
            exit;
          end;
        dirp^.dd_size := bytes;
        dirp^.dd_nextoff := 0;
      end;
    dp := Pdirent(ptruint(dirp^.dd_buf)+dirp^.dd_nextoff);
    inc(dirp^.dd_nextoff,dp^.d_reclen);
    inc(dirp^.dd_loc,dp^.d_reclen);
  until dp^.d_fileno <> 0; // Don't show deleted files
  Fpreaddir := dp;
end;


{*****************************************************************************
        --- Process:Process & program handling - related calls ---
*****************************************************************************}

procedure Fpexit(status : cint); [public, alias : 'FPC_SYSC_EXIT'];

begin
  do_syscall(syscall_nr_exit_group,status);
end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}


{$ifdef cpusparc}
procedure Fprt_sigreturn_stub;assembler;nostackframe;
asm
  mov syscall_nr_rt_sigreturn,%g1
  ta  0x10
end;
{$endif cpusparc}

{$if defined(cpui386) or defined(cpuarm) or defined(cpux86_64)}
  {$define NEED_USER_TRAMPOLINE}
{$endif}
{$if defined(cpui386) or defined(cpuarm)}
  {$define NEED_USER_TRAMPOLINE_RT_DIFFERENT}
{$endif}

{$ifdef NEED_USER_TRAMPOLINE}

procedure linux_restore; cdecl; nostackframe; assembler;
{$ifdef cpuarm}
asm
  swi syscall_nr_sigreturn
end;
{$endif}
{$ifdef cpui386}
asm
  popl %eax
  movl $syscall_nr_sigreturn, %eax
  int $0x80
end;
{$endif}
{$ifdef cpux86_64}
asm
  movq $syscall_nr_rt_sigreturn, %rax
  syscall
end;
{$endif}

{$endif}

{$ifdef NEED_USER_TRAMPOLINE_RT_DIFFERENT}

procedure linux_restore_rt; cdecl; nostackframe; assembler;
{$ifdef cpuarm}
asm
  swi syscall_nr_rt_sigreturn
end;
{$endif}
{$ifdef cpui386}
asm
  movl $syscall_nr_rt_sigreturn, %eax
  int $0x80
end;
{$endif}

{$endif}

function Fpsigaction(sig: cint; new_action, old_action: psigactionrec): cint; [public, alias : 'FPC_SYSC_SIGACTION'];
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
begin
{$ifdef cpusparc}
  { Sparc has an extra stub parameter }
  Fpsigaction:=do_syscall(syscall_nr_rt_sigaction,TSysParam(sig),TSysParam(new_action),TSysParam(old_action),TSysParam(ptruint(@Fprt_sigreturn_stub)-8),TSysParam(8));
{$else cpusparc}
{$ifdef NEED_USER_TRAMPOLINE}
  if new_action <> nil then
  begin
    // a different stack (SA_ONSTACK) requires sa_restorer field
    if (new_action^.sa_flags and (SA_RESTORER or SA_ONSTACK)) = 0 then
    begin
      new_action^.sa_flags := new_action^.sa_flags or SA_RESTORER;
{$ifdef NEED_USER_TRAMPOLINE_RT_DIFFERENT}
      if (new_action^.sa_flags and SA_SIGINFO) <> 0 then
        new_action^.sa_restorer := @linux_restore_rt
      else
{$endif}
        new_action^.sa_restorer := @linux_restore;
    end;
  end;
{$endif}
  Fpsigaction:=do_syscall(syscall_nr_rt_sigaction,TSysParam(sig),TSysParam(new_action),TSysParam(old_action),TSysParam(8));
{$endif cpusparc}
end;


function Fpftruncate(fd : cint; flength : off_t): cint; [public, alias : 'FPC_SYSC_FTRUNCATE'];
{ See notes lseek. This one is completely similar for the parameter (but
doesn't have the returnvalue 64-bit problem)}

begin
{$if defined(cpu64)}
 Fpftruncate:=Do_syscall(syscall_nr_ftruncate,TSysParam(fd),TSysParam(flength));
{$else}
 Fpftruncate:=Do_syscall(syscall_nr_ftruncate64,TSysParam(fd),
   {$ifdef FPC_ABI_EABI}      0,  { align parameters as required with dummy } {$endif FPC_ABI_EABI}
   {$ifdef FPC_BIG_ENDIAN}    tsysparam(hi(flength)),tsysparam(lo(flength)){$endif}
   {$ifdef FPC_LITTLE_ENDIAN} tsysparam(lo(flength)),tsysparam(hi(flength)){$endif}
   );
{$endif}
end;

function Fpfstat(fd : cint; var sb : stat): cint;  [public, alias : 'FPC_SYSC_FSTAT'];

begin
{$if defined(cpu64)}
  FpFStat:=do_SysCall(syscall_nr_fstat,TSysParam(fd),TSysParam(@sb));
{$else}
  FpFStat:=do_SysCall(syscall_nr_fstat64,TSysParam(fd),TSysParam(@sb));
{$endif}
end;


{$ifndef FPC_SYSTEM_HAS_FPFORK}
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
{$endif FPC_SYSTEM_HAS_FPFORK}

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
{$ifdef WAIT4}
 FpWaitPID:=do_syscall(syscall_nr_Wait4,PID,TSysParam(Stat_loc),options,0);
{$else WAIT4}
 FpWaitPID:=do_syscall(syscall_nr_WaitPID,PID,TSysParam(Stat_loc),options);
{$endif WAIT4}
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

(* overloaded
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
*)

Function FpDup(fildes:cint):cint; [public, alias : 'FPC_SYSC_DUP'];

begin
  Fpdup:=Do_syscall(syscall_nr_dup,TSysParam(fildes));
end;

Function FpDup2(fildes,fildes2:cint):cint; [public, alias : 'FPC_SYSC_DUP2'];

begin
 Fpdup2:=do_syscall(syscall_nr_dup2,TSysParam(fildes),TSysParam(fildes2));
end;


type
  tmmapargs = packed record
    address : TSysParam;
    size    : TSysParam;
    prot    : TSysParam;
    flags   : TSysParam;
    fd      : TSysParam;
    offset  : TSysParam;
  end;

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
{$ifdef MMAP2}
begin
  {$message warning need mmap64 syscall, hi(off) not used}
  Fpmmap:= pointer(do_syscall(syscall_nr_mmap2,TSysParam(adr),TSysParam(len),
    TSysParam(prot),TSysParam(flags),TSysParam(fd),TSysParam(lo(off))));
end;
{$else MMAP2}
begin
  {$message warning need mmap64 syscall, hi(off) not used}
  Fpmmap:= pointer(do_syscall(syscall_nr_mmap,TSysParam(adr),TSysParam(len),
    TSysParam(prot),TSysParam(flags),TSysParam(fd),TSysParam(lo(off))));
end;
{$endif MMAP2}
{$endif OLDMMAP}


Function Fpmunmap(adr:pointer;len:size_t):cint; [public, alias :'FPC_SYSC_MUNMAP'];
begin
  Fpmunmap:=do_syscall(syscall_nr_munmap,TSysParam(Adr),TSysParam(Len));
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

Function FpIOCtl(fd:cint;request:TIOCtlRequest;Data: Pointer):cint;  [public, alias : 'FPC_SYSC_IOCTL'];
// This was missing here, instead hardcoded in Do_IsDevice
begin
  FpIOCtl:=do_SysCall(syscall_nr_ioctl,tsysparam(fd),tsysparam(Request),TSysParam(data));
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


function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint; [public, alias : 'FPC_SYSC_SIGPROCMASK'];

{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  FPsigprocmask:=do_syscall(syscall_nr_rt_sigprocmask,TSysParam(how),TSysParam(nset),TSysParam(oset),TSysParam(8));
end;

Function FpNanoSleep(req : ptimespec;rem : ptimespec):cint; [public, alias : 'FPC_SYSC_NANOSLEEP'];
begin
  FpNanoSleep:=Do_SysCall(syscall_nr_nanosleep,TSysParam(req),TSysParam(rem));
end;

function fpgetcwd(path : pchar; siz:tsize):pchar;  [public, alias : 'FPC_SYSC_GETCWD'];

begin
  fpgetcwd:=pchar(Do_Syscall(Syscall_nr_getcwd,TSysParam(Path),TSysParam(siz)));
end;

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; [public, alias: 'FPC_SYSC_GETTIMEOFDAY'];

begin
 fpgettimeofday:=do_syscall(syscall_nr_gettimeofday,TSysParam(tp),TSysParam(tzp));
end;


function FpGetRLimit(resource : cInt; rlim : PRLimit) : cInt;
begin
  FpGetRLimit := do_syscall(syscall_nr_getrlimit,
    TSysParam(resource), TSysParam(rlim));
end;

{$ifdef HAS_UGETRLIMIT}
function fpugetrlimit(resource : cInt; rlim : PRLimit) : cInt;
begin
  FpUGetRLimit := do_syscall(syscall_nr_ugetrlimit,
    TSysParam(resource), TSysParam(rlim));
end;
{$endif}

{$if defined(cpupowerpc)}
{ fpprctl() call }
function fpprctl(option : cint; const arg : ptruint) : cint;
begin
  fpprctl := do_syscall(syscall_nr_prctl, option, arg);
end;
{$endif}

