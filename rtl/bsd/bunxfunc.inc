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
{$i ossysch.inc}	// external interface to syscalls in system unit.
{$i genfuncs.inc}	// generic calls. (like getenv)

Const 			// OS specific parameters for general sigset behaviour
   SIG_MAXSIG      = 128;	// highest signal version
   wordsinsigset   = 4;		// words in sigset_t
   ln2bitsinword   = 5;         { 32bit : ln(32)/ln(2)=5 } 

   ln2bitmask	   = 2 shl ln2bitsinword - 1;

{$I gensigset.inc}     // general sigset funcs implementation. 

{  $ ifndef ver1_0}
Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt; external name 'FPC_SYSC_SIGPROGMASK';
{   $ endif}

Function FPKill(Pid:pid_t;Sig:cint):cint;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}

begin
 FPkill:=do_syscall(syscall_nr_kill,pid,sig);
// if kill<0 THEN
//  Kill:=0;
end;

function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint; [public, alias: 'FPC_SYSC_SIGPROCMASK'];
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  FPsigprocmask:=do_syscall(syscall_nr_sigprocmask,longint(how),longint(nset),longint(oset));
end;

Function FPSigPending(var nset: sigset_t):cint;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
begin
  FPsigpending:=do_syscall(syscall_nr_sigpending,longint(@nset));
end;

function FPsigsuspend(const sigmask:sigset_t):cint;
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  FPsigsuspend:= do_syscall(syscall_nr_sigsuspend,longint(@sigmask));
end;

Type // implementation side for now. Should move to BSD unit.
  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

Const   ITimer_Real    =0;
  ITimer_Virtual =1;
  ITimer_Prof    =2;

Function SetITimer(Which : Longint;Const value : ItimerVal; var VarOValue:ItimerVal):Longint;

Begin
  SetItimer:=Do_Syscall(syscall_nr_setitimer,Which,Longint(@Value),longint(@varovalue));
End;

Function GetITimer(Which : Longint;Var value : ItimerVal):Longint;

Begin
  GetItimer:=Do_Syscall(syscall_nr_getItimer,Which,Longint(@value));
End;

Function FPalarm(Seconds: cuint):cuint;

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
 FPAlarm:=oitv.it_value.tv_sec;
End;

function sigblock(mask:cuint):cint;
{Depreciated, but used by pause.}

var nset,oset: sigset_t;

begin
 FPsigemptyset(nset); 
 nset[0]:=mask;
 sigblock:= FPsigprocmask(SIG_BLOCK,@nset,@oset);   // SIG_BLOCK=1
 if sigblock=0 Then
  sigblock:=oset[0];
end;

function sigpause(sigmask:cint):cint;
{Depreciated, but used by pause.}

var nset: sigset_t;

begin
 FPsigemptyset(nset); 
 nset[0]:=sigmask;
 sigpause:= FPsigsuspend(nset);
end;

function FPpause:cint;

begin
  FPpause:=sigpause(sigblock(cuint(0)));
end;

function FPsleep(seconds:cuint):cuint;

var time_to_sleep,time_remaining : timespec;

begin
	{
	 * Avoid overflow when `seconds' is huge.  This assumes that
	 * the maximum value for a time_t is >= INT_MAX.
	 }
	if seconds > high(cint) Then
		FPsleep:= (seconds - high(cint)) + FPsleep(HIGH(cint));

	time_to_sleep.tv_sec := seconds;
	time_to_sleep.tv_nsec := 0;
	if (FPnanosleep(time_to_sleep, time_remaining) <> -1) Then
	 Exit(0);
	if (geterrno <> ESysEINTR) Then
	 Exit (seconds);		     { best guess }
	FPsleep:= time_remaining.tv_sec;
	if   (time_remaining.tv_nsec <> 0) Then 
         inc(FPsleep);
End;

function FPuname(var name:utsname):cint; [public,alias:'FPC_SYSC_UNAME'];

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

        if (FPsysctl(@mib, 2, pz, @len, NIL, 0) = -1) Then
           Begin
                if (geterrno = ESysENOMEM) Then
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
        FPUname:=rval;
end;

function GetDomainName(Name:PChar; NameLen:Cint):cint; [public,alias:'FPC_SYSC_GETDOMAINNAME'];

Const Mib_GetDomainName : array[0..1] of cint=(CTL_KERN,KERN_NISDOMAINNAME);

VAR
	tsize : size_t;
begin
	tsize := namelen;
	if (FPsysctl(@Mib_GetDomainname, 2, name, @tsize, NIL, 0) = -1) Then
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
	if (FPsysctl(@Mib_GetHostName, 2, name, @tsize, NIL, 0) = -1) Then
	  GetHostName:=-1
	Else
	  GetHostName:=0;
End;

const WAIT_ANY = -1;

function FPwait(var stat_loc:cint): pid_t;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
 FPWait:=do_syscall(syscall_nr_WaitPID,WAIT_ANY,longint(@Stat_loc),0,0);
end;

//function FPgetpid : pid_t;

// begin
//  FPgetpid:=do_syscall(syscall_nr_getpid);
// end;

function FPgetppid : pid_t;

begin
 FPgetppid:=do_syscall(syscall_nr_getppid);
end;

function FPgetuid : uid_t;

begin
 FPgetuid:=do_syscall(syscall_nr_getuid);
end;

function FPgeteuid : uid_t;

begin
 FPgeteuid:=do_syscall(syscall_nr_geteuid);
end;

function FPgetgid : gid_t;

begin
 FPgetgid:=do_syscall(syscall_nr_getgid);
end;

function FPgetegid : gid_t;

begin
 FPgetegid:=do_syscall(syscall_nr_getegid);
end;

function FPsetuid(uid : uid_t): cint;

begin
 FPsetuid:=do_syscall(syscall_nr_setuid,uid);
end;

function FPsetgid(gid : gid_t): cint;

begin
 FPsetgid:=do_syscall(syscall_nr_setgid,gid);
end;

// type tgrparr=array[0..0] of gid_t;

function FPgetgroups(gidsetsize : cint; var grouplist:tgrparr): cint;

begin
 FPgetgroups:=do_syscall(syscall_nr_getgroups,gidsetsize,longint(@grouplist));
end;

function FPgetpgrp : pid_t;

begin
 FPgetpgrp:=do_syscall(syscall_nr_getpgrp);
end;

function FPsetsid : pid_t;

begin
 FPsetsid:=do_syscall(syscall_nr_setsid);
end;

Function FPumask(cmask:mode_t):mode_t;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 FPumask:=Do_syscall(syscall_nr_umask,cmask);
end;

Function FPlink(existing:pchar;newone:pchar):cint;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  FPLink:=Do_Syscall(syscall_nr_link,longint(existing),longint(newone));
end;

Function FPmkfifo(path:pchar;mode:mode_t):cint;

begin
  FPmkfifo:=do_syscall(syscall_nr_mkfifo,longint(path),longint(mode));
end;

Function FPchmod(path:pchar;mode:mode_t):cint;

begin
  FPchmod:=do_syscall(syscall_nr_chmod,longint(path),longint(mode));
end;

Function FPchown(path:pchar;owner:uid_t;group:gid_t):cint;

begin
  FPChOwn:=do_syscall(syscall_nr_chown,longint(path),longint(owner),longint(group));
end;

Function FPUtime(path:pchar;times:putimbuf):cint;

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
 FPutime:=do_syscall(syscall_nr_utimes,longint(path),longint(tvp));
end;

Function FPpipe(var fildes : tfildes):cint;

begin
 FPpipe:=do_syscall(syscall_nr_pipe,longint(@fildes));
end;

function FPfcntl(fildes:cint;Cmd:cint;Arg:cint):cint;

begin
 FPfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,arg);
end;

function FPfcntl(fildes:cint;Cmd:cint;var Arg:flock):cint;

begin
 FPfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,longint(@arg));
end;

function FPfcntl(fildes:cint;Cmd:cint):cint;

begin
 FPfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd);
end;

function FPexecve(path:pchar;argv:ppchar;envp:ppchar):cint;

Begin
  FPexecve:=do_syscall(syscall_nr_Execve,longint(path),longint(argv),longint(envp));
End;

function FPexecv(path:pchar;argv:ppchar):cint;

Begin
  FPexecv:=do_syscall(syscall_nr_Execve,longint(path),longint(argv),longint(envp));
End;

CONST RUSAGE_SELF	= 0;
      RUSAGE_CHILDREN   = -1;

function FPgetrusage(who:cint;var ru : rusage):cint;

begin
 FPgetrusage:=do_syscall(syscall_nr_getrusage,longint(who),longint(@ru));
end;

function FPtimes(var buffer : tms):clock_t;

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

	if (FPgetrusage(RUSAGE_SELF, ru) < 0) Then
	    exit(clock_t(-1));
	buffer.tms_utime := CONVTCK(ru.ru_utime);
	buffer.tms_stime := CONVTCK(ru.ru_stime);
	if (FPgetrusage(RUSAGE_CHILDREN, ru) < 0) Then
 	    exit(clock_t(-1));
	buffer.tms_cutime := CONVTCK(ru.ru_utime);
	buffer.tms_cstime := CONVTCK(ru.ru_stime);
	if do_syscall(syscall_nr_gettimeofday,longint(@t),0)<>0 Then
		    exit(clock_t(-1));
	FPtimes:=clock_t(CONVTCK(t));
end;


{
 $Log$
 Revision 1.2  2003-01-05 19:10:05  marco
  * Small sigprocmask fix

 Revision 1.1  2003/01/05 19:01:28  marco
  * FreeBSD compiles now with baseunix mods.

 Revision 1.11  2002/11/14 13:25:27  marco
  * Fix setitimer.

 Revision 1.10  2002/11/14 12:34:20  marco
  * took out the generic sethandling.

 Revision 1.9  2002/11/13 18:15:08  marco
  * sigset functions more flexible, small changes to FPtime

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
