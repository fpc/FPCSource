{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Calls needed for the POSIX unit, but not for system.
    Some calls that can be used for both Linux and *BSD will be
    moved to a /unix/ includedfile later.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$i syscallh.inc}	// do_syscall declarations themselves
{$i sysnr.inc}		// syscall numbers.
{$i bsdsysch.inc}	// external interface to syscalls in system unit.
{$i posixunx.inc}	// generic calls. (like getenv)

Function sys_Kill(Pid:pid_t;Sig:cint):cint;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}

begin
 sys_kill:=do_syscall(syscall_nr_kill,pid,sig);
// if kill<0 THEN
//  Kill:=0;
end;

Const 
   SIG_MAXSIG      = 128;
   wordsinsigset   = 4;
   ln2bitsinword   = 5;    { 32bit : ln(32)/ln(2)=5 } 

   ln2bitmask	   = 2 shl ln2bitsinword -1;
   
function sys_sigaddset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr ln2bitsinword]:=_set[(signo-1) shr ln2bitsinword] OR (1 shl ((signo-1) and ln2bitmask));
   sys_sigaddset:=0;
End;   

function sys_sigdelset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr ln2bitsinword]:=_set[(signo-1) shr ln2bitsinword] AND NOT (1 shl ((signo-1) and ln2bitmask));
   sys_sigdelset:=0;
End;

function sys_sigemptyset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO _set[i]:=0;
  sys_sigemptyset:=0;
End;

function sys_sigfillset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to wordsinsignset DO _set[i]:=NOT 0;
  sys_sigfillset:=0;
End;

function sys_sigismember(const _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
    if ((_set[(signo-1) shr ln2bitsinword]) and (1 shl ((signo-1) and ln2bitmask)))>0 Then
     sys_sigismember:=1
    else 
     sys_sigismember:=0;
End;      

function sys_SigProcMask(how:cint;var _set : sigset_t; var _oset : sigset_t):cint;
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  sys_sigprocmask:=do_syscall(syscall_nr_sigprocmask,longint(how),longint(@_set),longint(@_oset));
end;

Function sys_SigPending(var _set: sigset_t):cint;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
begin
  sys_sigpending:=do_syscall(syscall_nr_sigpending,longint(@_set));
end;

function sys_sigsuspend(const sigmask:sigset_t):cint;
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  sys_sigsuspend:= do_syscall(syscall_nr_sigsuspend,longint(@sigmask));
end;

Type // implementation side for now. Should move to BSD unit.
  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

Const   ITimer_Real    =0;
  ITimer_Virtual =1;
  ITimer_Prof    =2;


Function SetITimer(Which : Longint;Const value : ItimerVal; VarOValue:ItimerVal):Longint;

Begin
 SetItimer:=Do_Syscall(syscall_nr_setitimer,Which,Longint(@Value),longint(@value));
End;

Function GetITimer(Which : Longint;Var value : ItimerVal):Longint;

Begin
 GetItimer:=Do_Syscall(syscall_nr_getItimer,Which,Longint(@value));
End;

Function sys_alarm(Seconds: cuint):cuint;

Var it,oitv : Itimerval;

Begin
//      register struct itimerval *itp = &it;

 it.it_interval.tv_sec:=0;
 it.it_interval.tv_usec:=0;
 it.it_value.tv_sec:=seconds;
 it.it_value.tv_usec:=0;
 If SetITimer(ITIMER_REAL,it,oitv)<0 Then
   Exit(-1);

 if oitv.it_value.tv_usec<>0 Then
   Inc(oitv.it_value.tv_sec);
 sys_Alarm:=oitv.it_value.tv_sec;
End;

function sigblock(mask:cuint):cint;
{Depreciated, but used by pause.}

var nset,oset: sigset_t;

begin
 sys_sigemptyset(nset); 
 nset[0]:=mask;
 sigblock:= sys_sigprocmask(SIG_BLOCK,nset,oset);   // SIG_BLOCK=1
 if sigblock=0 Then
  sigblock:=oset[0];
end;

function sigpause(sigmask:cint):cint;
{Depreciated, but used by pause.}

var nset: sigset_t;

begin
 sys_sigemptyset(nset); 
 nset[0]:=sigmask;
 sigpause:= sys_sigsuspend(nset);
end;

function sys_pause:cint;

begin
  sys_pause:=sigpause(sigblock(cuint(0)));
end;


function sys_sleep(seconds:cuint):cuint;

var time_to_sleep,time_remaining : timespec;

begin
	{
	 * Avoid overflow when `seconds' is huge.  This assumes that
	 * the maximum value for a time_t is >= INT_MAX.
	 }
	if seconds > high(cint) Then
		sys_sleep:= (seconds - high(cint)) + sys_sleep(HIGH(cint));

	time_to_sleep.tv_sec := seconds;
	time_to_sleep.tv_nsec := 0;
	if (sys_nanosleep(time_to_sleep, time_remaining) <> -1) Then
	 Exit(0);
	if (geterrno <> sys_EINTR) Then
	 Exit (seconds);		     { best guess }
	sys_sleep:= time_remaining.tv_sec;
	if   (time_remaining.tv_nsec <> 0) Then 
         inc(sys_sleep);
End;

function sys_uname(var name:utsname):cint; [public,alias:'FPC_SYSC_UNAME'];

Var
        mib  : array[0..1] of cint;
        rval : cint;
        len  : size_t;
        i    : longint;
        oerrno : cint;

procedure Doone(pz:pchar;pzsize:cint;val1,val2:cint);

Begin
        mib[0] := val1;
        mib[1] := val2;
        len    := pzsize;
        oerrno := geterrno;

        if (sys_sysctl(@mib, 2, pz, @len, NIL, 0) = -1) Then
           Begin
                if (geterrno = sys_ENOMEM) Then
                        seterrno(oerrno)
                else
                        rval := -1;
           End;
         pz[pzsize- 1] := #0;
End;

Begin
        rval := 0;
        DoOne(@name.sysname,sizeof(name.sysname),CTL_KERN,KERN_OSTYPE);
        DoOne(@name.nodename,sizeof(name.nodename),CTL_KERN,KERN_HOSTNAME);
        DoOne(@name.release,sizeof(name.release),CTL_KERN,KERN_OSRELEASE);
        { The version may have newlines in it, turn them into spaces. }
        DoOne(@name.version,sizeof(name.version),CTL_KERN,KERN_VERSION);

        For I:=0 to sizeof(name.sysname)-2 Do
          If (name.version[i]=#13) or (name.version[i]=#9) Then
            name.version[i]:=' ';
        DoOne(@name.machine,sizeof(name.machine),CTL_HW,HW_MACHINE);
        sys_Uname:=rval;
end;

function GetDomainName(Name:PChar; NameLen:Cint):cint; [public,alias:'FPC_SYSC_GETDOMAINNAME'];

Const Mib_GetDomainName : array[0..1] of cint=(CTL_KERN,KERN_NISDOMAINNAME);

VAR
	tsize : size_t;
begin
	tsize := namelen;
	if (sys_sysctl(@Mib_GetDomainname, 2, name, @tsize, NIL, 0) = -1) Then
          GetDomainName:=-1
        Else
          GetDomainName:=0;
end;          

function GetHostName(Name:PChar; NameLen:Cint):cint;[public,alias:'FPC_SYSC_GETHOSTNAME'];

Const Mib_GetHostName : array[0..1] of cint=(CTL_KERN,KERN_HOSTNAME);

Var
	tsize : size_t;
begin
	tsize := namelen;
	if (sys_sysctl(@Mib_GetHostName, 2, name, @tsize, NIL, 0) = -1) Then
	  GetHostName:=-1
	Else
	  GetHostName:=0;
End;

const WAIT_ANY = -1;

function sys_wait(var stat_loc:cint): pid_t;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
 sys_Wait:=do_syscall(syscall_nr_WaitPID,WAIT_ANY,longint(@Stat_loc),0,0);
end;

//function sys_getpid : pid_t;

// begin
//  sys_getpid:=do_syscall(syscall_nr_getpid);
// end;

function sys_getppid : pid_t;

begin
 sys_getppid:=do_syscall(syscall_nr_getppid);
end;

function sys_getuid : uid_t;

begin
 sys_getuid:=do_syscall(syscall_nr_getuid);
end;

function sys_geteuid : uid_t;

begin
 sys_geteuid:=do_syscall(syscall_nr_geteuid);
end;

function sys_getgid : gid_t;

begin
 sys_getgid:=do_syscall(syscall_nr_getgid);
end;

function sys_getegid : gid_t;

begin
 sys_getegid:=do_syscall(syscall_nr_getegid);
end;

function sys_setuid(uid : uid_t): cint;

begin
 sys_setuid:=do_syscall(syscall_nr_setuid,uid);
end;

function sys_setgid(gid : gid_t): cint;

begin
 sys_setgid:=do_syscall(syscall_nr_setgid,gid);
end;

// type tgrparr=array[0..0] of gid_t;

function sys_getgroups(gidsetsize : cint; var grouplist:tgrparr): cint;

begin
 sys_getgroups:=do_syscall(syscall_nr_getgroups,gidsetsize,longint(@grouplist));
end;

function sys_getpgrp : pid_t;

begin
 sys_getpgrp:=do_syscall(syscall_nr_getpgrp);
end;

function sys_setsid : pid_t;

begin
 sys_setsid:=do_syscall(syscall_nr_setsid);
end;

Function sys_umask(cmask:mode_t):mode_t;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 sys_umask:=Do_syscall(syscall_nr_umask,cmask);
end;

Function sys_link(existing:pchar;_new:pchar):cint;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  sys_Link:=Do_Syscall(syscall_nr_link,longint(existing),longint(_new));
end;

Function sys_mkfifo(path:pchar;mode:mode_t):cint;

begin
  sys_mkfifo:=do_syscall(syscall_nr_mkfifo,longint(path),longint(mode));
end;

Function sys_chmod(path:pchar;mode:mode_t):cint;

begin
  sys_chmod:=do_syscall(syscall_nr_chmod,longint(path),longint(mode));
end;

Function sys_chown(path:pchar;owner:uid_t;group:gid_t):cint;

begin
  sys_ChOwn:=do_syscall(syscall_nr_chown,longint(path),longint(owner),longint(group));
end;

Function sys_Utime(path:pchar;times:putimbuf):cint;

var tv  : array[0..1] of timeval;
    tvp : ^timeval;

begin
 if times=nil Then
   tvp:=nil
 else
   begin
    tv[0].tv_sec :=times^.actime;
    tv[1].tv_sec :=times^.modtime;
    tv[0].tv_usec:=0;
    tv[1].tv_usec:=0; 
    tvp:=@tv;
   end;
 sys_utime:=do_syscall(syscall_nr_utimes,longint(path),longint(tvp));
end;

Function sys_pipe(var fildes : tfildes):cint;

begin
 sys_pipe:=do_syscall(syscall_nr_pipe,longint(@fildes));
end;

function sys_fcntl(fildes:cint;Cmd:cint;Arg:cint):cint;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,arg);
end;

function sys_fcntl(fildes:cint;Cmd:cint;var Arg:flock):cint;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,longint(@arg));
end;

function sys_fcntl(fildes:cint;Cmd:cint):cint;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd);
end;

function sys_execve(path:pchar;argv:ppchar;envp:ppchar):cint;

Begin
  sys_execve:=do_syscall(syscall_nr_Execve,longint(path),longint(argv),longint(envp));
End;

function sys_execv(path:pchar;argv:ppchar):cint;

Begin
  sys_execv:=do_syscall(syscall_nr_Execve,longint(path),longint(argv),longint(envp));
End;

CONST RUSAGE_SELF	= 0;
      RUSAGE_CHILDREN   = -1;

function sys_getrusage(who:cint;var ru : rusage):cint;

begin
 sys_getrusage:=do_syscall(syscall_nr_getrusage,longint(who),longint(@ru));
end;

function sys_times(var buffer : tms):clock_t;

var ru : rusage;
    t  : timeval;

CONST CLK_TCK=128;

function CONVTCK(r:timeval):clock_t;	
{
 * Convert usec to clock ticks; could do (usec * CLK_TCK) / 1000000,
 * but this would overflow if we switch to nanosec.
 }
begin
 CONVTCK:=(r.tv_sec * CLK_TCK + r.tv_usec DIV (1000000 DIV CLK_TCK));
end;

begin

	if (sys_getrusage(RUSAGE_SELF, ru) < 0) Then
	    exit(clock_t(-1));
	buffer.tms_utime := CONVTCK(ru.ru_utime);
	buffer.tms_stime := CONVTCK(ru.ru_stime);
	if (sys_getrusage(RUSAGE_CHILDREN, ru) < 0) Then
 	    exit(clock_t(-1));
	buffer.tms_cutime := CONVTCK(ru.ru_utime);
	buffer.tms_cstime := CONVTCK(ru.ru_stime);
	if do_syscall(syscall_nr_gettimeofday,longint(@t),0)<>0 Then
		    exit(clock_t(-1));
	sys_times:=clock_t(CONVTCK(t));
end;


{
 $Log$
 Revision 1.9  2002-11-13 18:15:08  marco
  * sigset functions more flexible, small changes to sys_time

 Revision 1.8  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.7  2002/10/27 11:58:29  marco
  * Modifications from Saturday.

 Revision 1.6  2002/10/26 18:27:51  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.5  2002/10/25 15:46:48  marco
  * Should be alias.

 Revision 1.4  2002/09/08 16:20:27  marco
  * Forgot external name's

 Revision 1.3  2002/09/08 16:11:59  marco
  * Added GetDomainName and that other one ..

 Revision 1.2  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.1  2002/08/21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.1  2002/08/08 11:39:30  marco
  * Initial versions, to allow support for uname in posix.pp
}
