{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Calls needed for the POSIX unit.    

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$i syscallh.inc}
{$i sysnr.inc}
{$i bsdsysch.inc}

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
   SIG_MAXSIG = 128;
   
function sys_sigaddset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr 5]:=_set[(signo-1) shr 5] OR (1 shl ((signo-1) and 31));
   sys_sigaddset:=0;
End;   

function sys_sigdelset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr 5]:=_set[(signo-1) shr 5] AND NOT (1 shl ((signo-1) and 31));
   sys_sigdelset:=0;
End;

function sys_sigemptyset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to 3 DO _set[i]:=0;
  sys_sigemptyset:=0;
End;

function sys_sigfillset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to 3 DO _set[i]:=NOT 0;
  sys_sigfillset:=0;
End;

function sys_sigismember(const _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
    if ((_set[(signo-1) shr 5]) and (1 shl ((signo-1) and 31)))>0 Then
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

 it.it_interval.sec:=0;
 it.it_interval.usec:=0;
 it.it_value.sec:=seconds;
 it.it_value.usec:=0;
 If SetITimer(ITIMER_REAL,it,oitv)<0 Then
   Exit(-1);

 if oitv.it_value.usec<>0 Then
   Inc(oitv.it_value.sec);
 sys_Alarm:=oitv.it_value.sec;
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


{
 $Log$
 Revision 1.6  2002-10-26 18:27:51  marco
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



