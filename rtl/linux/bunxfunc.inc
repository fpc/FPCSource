{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Calls needed for the baseunix unit, but not for system.
    Some calls that can be used for both Linux and *BSD will be
    moved to a /unix/ includedfile later.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{i ostypes.inc}

{$ifndef FPC_USE_LIBC}
{$i syscallh.inc}       // do_syscall declarations themselves
{$i sysnr.inc}          // syscall numbers.
{$i ossysch.inc}        // external interface to syscalls in system unit.
{$endif}
//{$i genfuncs.inc}

{$i bunxmacr.inc}       // macros.

{$I gensigset.inc}     // general sigset funcs implementation.
{$I genfdset.inc}

{$ifndef FPC_USE_LIBC}
Function fpKill(Pid:pid_t;Sig:cint):cint;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}

begin
 fpkill:=do_syscall(syscall_nr_kill,TSysParam(pid),TSysParam(sig));
// if kill<0 THEN
//  Kill:=0;
end;

{overload}
Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt; external name 'FPC_SYSC_SIGPROGMASK';

Function fpSigProcMask(how:cint;nset : pSigSet; oset : pSigSet):cint; [public, alias : 'FPC_SYSC_SIGPROGMASK'];
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  fpsigprocmask:=do_syscall(syscall_nr_rt_sigprocmask,TSysParam(how),TSysParam(nset),TSysParam(oset));
end;


Function fpSigPending(var nset: TSigSet):cint;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
begin
  fpsigpending:=do_syscall(syscall_nr_rt_sigpending,TSysParam(@nset));
end;

function fpsigsuspend(const sigmask:TSigSet):cint;
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  fpsigsuspend:= do_syscall(syscall_nr_rt_sigsuspend,TSysParam(@sigmask));
end;

Type
  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

Const   ITimer_Real    =0;
        ITimer_Virtual =1;
        ITimer_Prof    =2;

Function SetITimer(Which : Longint;Const value : ItimerVal; var VarOValue:ItimerVal):Longint;

Begin
  SetItimer:=Do_Syscall(syscall_nr_setitimer,Which,TSysParam(@Value),TSysParam(@varovalue));
End;

Function GetITimer(Which : Longint;Var value : ItimerVal):Longint;

Begin
  GetItimer:=Do_Syscall(syscall_nr_getItimer,Which,TSysParam(@value));
End;

Function fpalarm(Seconds: cuint):cuint;

Var it,oitv : Itimerval;
    retval  : cuint;

Begin
//      register struct itimerval *itp = &it;

 it.it_interval.tv_sec:=0;
 it.it_interval.tv_usec:=0;
 it.it_value.tv_usec:=0;
 it.it_value.tv_sec:=seconds;
 If SetITimer(ITIMER_REAL,it,oitv)<0 Then
   Exit(0);                     // different from *BSD!

 retval:= oitv.it_value.tv_usec;
 if retval<>0 Then
   inc(retval);
 fpAlarm:=retval;
End;

// The following versions are for internal use _ONLY_
// This because it works for the first 32 signals _ONLY_, but that
// is enough since they are depreciated, and for legacy applications
// anyway.

function sigblock(mask:cuint):cint;

var nset,oset: TSigSet;

begin
 fpsigemptyset(nset);
 // fpsigaddset(nset,mask);   needs _mask_
 nset[0]:=mask;
 sigblock:= fpsigprocmask(SIG_BLOCK,@nset,@oset);   // SIG_BLOCK=1
 if sigblock=0 Then
  sigblock:=oset[0];
end;

function sigpause(sigmask:cint):cint;

var nset: TSigSet;

begin
 fpsigemptyset(nset);
 nset[0]:=sigmask;
 sigpause:= fpsigsuspend(nset);
end;

function fppause:cint;

begin
  fppause:=sigpause(sigblock(cuint(0)));
end;

function fpsleep(seconds:cuint):cuint;
{see comments in libc}

var time_to_sleep,time_remaining : timespec;
    nset,oset  : TSigSet;
    oerrno     : cint;
    oact       : sigactionrec;

begin
        time_to_sleep.tv_sec := seconds;
        time_to_sleep.tv_nsec := 0;
         fpsigemptyset(nset);
         fpsigaddset  (nset,SIGCHLD);
         if fpsigprocmask(SIG_BLOCK,@nset,@oset)=-1 Then
          exit(cuint(-1));
        if fpsigismember(oset,SIGCHLD)<>0 Then
          Begin
            fpsigemptyset(nset);
            fpsigaddset  (nset,SIGCHLD);
            if fpsigaction(SIGCHLD,NIL,@oact)<0 Then
              begin
                oerrno:=geterrno;
                fpsigprocmask(SIG_SETMASK,@oset,NIL);
                seterrno(oerrno);
                exit(cuint(-1));
              End;
            if oact.sa_handler=signalhandler(SIG_IGN) Then
             Begin
               fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining);
               oerrno:=geterrno;
               fpsigprocmask(SIG_SETMASK,@oset,NIL);
               seterrno(oerrno);
             End
            Else
             Begin
               fpsigprocmask(SIG_SETMASK,@oset,NIL);
               fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining)
             End;
          end
        else
            fpsleep:=fpnanosleep(@time_to_sleep, @time_remaining);
        if fpsleep<>0 Then
         if time_remaining.tv_nsec>=500000000 Then
          inc(fpsleep);
End;

function fpuname(var name:utsname):cint; [public,alias:'FPC_SYSC_UNAME'];

begin
  fpuname:=Do_Syscall(syscall_nr_uname,TSysParam(@name));
end;

Function fpGetDomainName(Name:PChar; NameLen:size_t):cint;

Var
        srec  : utsname;
        tsize : size_t;
Begin
        if fpuname(srec)<0 Then
          exit(-1);
        tsize:=strlen(@srec.domain[0]);
        if tsize>(namelen-1) Then
         tsize:=namelen-1;
        move(srec.domain[0],name[0],tsize);
        name[namelen-1]:=#0;
        fpgetDomainName:=0;
End;

function fpGetHostName(Name:PChar; NameLen:size_t):cint;

Var
        srec  : utsname;
        tsize : size_t;
begin
        if fpuname(srec)<0 Then
          exit(-1);
        tsize:=strlen(@srec.nodename[0]);
        if tsize>(namelen-1) Then
         tsize:=namelen-1;
        move(srec.nodename[0],name[0],tsize);
        name[namelen-1]:=#0;
        fpgethostName:=0;
End;

const WAIT_ANY = -1;

function fpwait(var stat_loc:cint): pid_t;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}

begin // actually a wait4() call with 4th arg 0.
 fpWait:=do_syscall(syscall_nr_Wait4,WAIT_ANY,TSysParam(@Stat_loc),0,0);
end;

//function fpgetpid : pid_t;

// begin
//  fpgetpid:=do_syscall(syscall_nr_getpid);
// end;

function fpgetppid : pid_t;

begin
 fpgetppid:=do_syscall(syscall_nr_getppid);
end;

function fpgetuid : uid_t;

begin
 fpgetuid:=do_syscall(syscall_nr_getuid);
end;

function fpgeteuid : uid_t;

begin
 fpgeteuid:=do_syscall(syscall_nr_geteuid);
end;

function fpgetgid : gid_t;

begin
 fpgetgid:=do_syscall(syscall_nr_getgid);
end;

function fpgetegid : gid_t;

begin
 fpgetegid:=do_syscall(syscall_nr_getegid);
end;

function fpsetuid(uid : uid_t): cint;

begin
 fpsetuid:=do_syscall(syscall_nr_setuid,uid);
end;

function fpsetgid(gid : gid_t): cint;

begin
 fpsetgid:=do_syscall(syscall_nr_setgid,gid);
end;

// type tgrparr=array[0..0] of gid_t;

function fpgetgroups(gidsetsize : cint; var grouplist:tgrparr): cint;

begin
 fpgetgroups:=do_syscall(syscall_nr_getgroups,gidsetsize,TSysParam(@grouplist));
end;

function fpgetpgrp : pid_t;

begin
 fpgetpgrp:=do_syscall(syscall_nr_getpgrp);
end;

function fpsetsid : pid_t;

begin
 fpsetsid:=do_syscall(syscall_nr_setsid);
end;

Function fpumask(cmask:mode_t):mode_t;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 fpumask:=Do_syscall(syscall_nr_umask,cmask);
end;

Function fplink(existing:pchar;newone:pchar):cint;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  fpLink:=Do_Syscall(syscall_nr_link,TSysParam(existing),TSysParam(newone));
end;

Function fpmkfifo(path:pchar;mode:mode_t):cint;

begin

fpmkfifo:=do_syscall(syscall_nr_mknod,TSysParam(path),TSysParam(mode or S_IFIFO),TSysParam(0));
end;

Function fpchmod(path:pchar;mode:mode_t):cint;

begin
  fpchmod:=do_syscall(syscall_nr_chmod,TSysParam(path),TSysParam(mode));
end;

Function fpchown(path:pchar;owner:uid_t;group:gid_t):cint;

begin
  fpChOwn:=do_syscall(syscall_nr_chown,TSysParam(path),TSysParam(owner),TSysParam(group));
end;

Function fpUtime(path:pchar;times:putimbuf):cint;

begin
 fputime:=do_syscall(syscall_nr_utime,TSysParam(path),TSysParam(times));
end;

Function fppipe(var fildes : tfildes):cint;

begin
 fppipe:=do_syscall(syscall_nr_pipe,TSysParam(@fildes));
end;

function fpfcntl(fildes:cint;Cmd:cint;Arg:cint):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,arg);
end;

function fpfcntl(fildes:cint;Cmd:cint;var Arg:flock):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd,TSysParam(@arg));
end;

function fpfcntl(fildes:cint;Cmd:cint):cint;

begin
 fpfcntl:=do_syscall(syscall_nr_fcntl,fildes,cmd);
end;

function fpexecve(path:pchar;argv:ppchar;envp:ppchar):cint;

Begin
  fpexecve:=do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(argv),TSysParam(envp));
End;

function fpexecv(path:pchar;argv:ppchar):cint;

Begin
  fpexecv:=do_syscall(syscall_nr_Execve,TSysParam(path),TSysParam(argv),TSysParam(envp));
End;

function fptimes(var buffer : tms):clock_t;
begin
  fptimes:=Do_syscall(syscall_nr_times,TSysParam(@buffer));
end;

function fpgetcwd(path : pchar; siz:size_t):pchar;

begin
  fpgetcwd:=pchar(Do_Syscall(Syscall_nr_getcwd,TSysParam(Path),TSysParam(siz)));
end;

Function fpSelect(N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}

{$ifdef cpui386}
Var
  SelectArray : Array[1..5] of TSysParam;
{$endif}

begin
{$ifdef cpui386}
  {$define bunxfunc_fpselect_implemented}
  SelectArray[1]:=n;
  SelectArray[2]:=TSysParam(Readfds);
  Selectarray[3]:=TSysParam(Writefds);
  selectarray[4]:=TSysParam(exceptfds);
  Selectarray[5]:=TSysParam(TimeOut);
  fpSelect:=do_syscall(syscall_nr_select,TSysParam(@selectarray));
{$endif cpui386}
{$ifdef cpux86_64}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr_select,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$endif cpux86_64}
{$ifdef cpuarm}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr__newselect,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$endif cpuarm}
{$ifdef cpupowerpc}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr__newselect,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$endif cpupowerpc}
{$ifdef cpusparc}
  {$define bunxfunc_fpselect_implemented}
  fpSelect:=do_syscall(syscall_nr__newselect,n,tsysparam(readfds),tsysparam(writefds),tsysparam(exceptfds),tsysparam(timeout));
{$endif cpusparc}
{$ifndef bunxfunc_fpselect_implemented}
  {$error Implement fpselect}
{$endif bunxfunc_fpselect_implemented}
end;

Function fpLstat(path:pchar;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(path),TSysParam(info));
end;

Function fpLstat(Filename: ansistring;Info:pstat):cint;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 fpLStat:=do_syscall(syscall_nr_lstat,TSysParam(pchar(filename)),TSysParam(info));
end;


function fpNice(N:cint):cint;
{
  Set process priority. A positive N means a lower priority.
  A negative N increases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

{$ifdef cpux86_64}
var
  oldprio : cint;
{$endif}  
begin
{$ifdef cpux86_64}
  oldprio:=fpGetPriority(Prio_Process,0);
  fpNice:=fpSetPriority(Prio_Process,0,oldprio+N);
  if fpNice=0 then
    fpNice:=fpGetPriority(Prio_Process,0);
{$else}  
  fpNice:=do_syscall(Syscall_nr_nice,N);
{$endif}
end;

Function fpGetPriority(Which,Who:cint):cint;
{
  Get Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on which, this is , respectively :
              Uid
              Pid
              Process Group id
   Errors are reported in linuxerror _only_. (priority can be negative)
}
begin
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     fpgetpriority:=-1;
     fpsetErrno(ESysEinval);
   end
  else
     fpGetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
end;

Function fpSetPriority(Which,Who,What:cint):cint;
{
 Set Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on value of which, this is, respectively :
              Uid
              Pid
              Process Group id
   what : A number between -20 and 20. -20 is most favorable, 20 least.
          0 is the default.
}
begin
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   fpseterrno(ESyseinval)  { We can save an interrupt here }
  else
   begin
     fpSetPriority:=do_syscall(Syscall_nr_Setpriority,which,who,what);
   end;
end;


Function fpSymlink(oldname,newname:pchar):cint;
{
  We need this for erase
}

begin
 fpsymlink:=do_syscall(syscall_nr_symlink,TSysParam(oldname),TSysParam(newname));
end;


{$endif}

{
 $Log$
 Revision 1.13  2004-11-14 12:21:08  marco
  * moved some calls from unix to baseunix. Darwin untested.

 Revision 1.12  2004/10/24 13:55:52  peter
   * fpselect for amd64,arm

 Revision 1.11  2004/10/13 20:47:12  florian
   + implemented fpselect for sparc

 Revision 1.10  2004/04/28 20:48:20  peter
   * ordinal-pointer conversions fixed

 Revision 1.9  2004/04/22 17:17:23  peter
   * x86-64 fixes

 Revision 1.8  2004/02/21 23:18:50  marco
  * powerpc select fix

 Revision 1.7  2003/12/30 15:43:20  marco
  * linux now compiles with FPC_USE_LIBC

 Revision 1.6  2003/11/19 14:04:34  marco
  * fix for sleep from johill

 Revision 1.5  2003/09/27 13:45:58  peter
   * fpnanosleep exported in baseunix
   * fpnanosleep has pointer arguments to be C compliant

 Revision 1.4  2003/09/17 11:24:46  marco
  * fixes for new macro's

 Revision 1.3  2003/09/14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.2  2003/01/05 19:16:45  marco
  * small fix

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.1  2002/11/14 16:48:39  marco
  * Initial version

 Revision 1.10  2002/11/14 12:34:20  marco
  * took out the generic sethandling.

 Revision 1.9  2002/11/13 18:15:08  marco
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
