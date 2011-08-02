{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Headerfile  of bunxfunc.inc (which should be calls needed for the
    Baseunix unit, but not for system)

    Some calls that can be used for both Linux and *BSD will be
    moved to a /unix/ includedfile later.

    Note: calls named in bunxh.inc aren't listed here.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

//Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt;
//Function FPKill(Pid:pid_t;Sig:cint):cint;
//function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint;
//Function FPSigPending(var nset: sigset_t):cint;
//function FPsigsuspend(const sigmask:sigset_t):cint;
Function SetITimer(Which : Longint;Const value : ItimerVal; var VarOValue:ItimerVal):Longint;
Function GetITimer(Which : Longint;Var value : ItimerVal):Longint;
//Function FPalarm(Seconds: cuint):cuint;
//function sigblock(mask:cuint):cint;
//function sigpause(sigmask:cint):cint;
//function FPpause:cint;
//function FPsleep(seconds:cuint):cuint;
//function FPuname(var name:utsname):cint;
//function FPwait(var stat_loc:cint): pid_t;
//function FPgetpid : pid_t;
//function FPgetppid : pid_t;
//function FPgetuid : uid_t;
//function FPgeteuid : uid_t;
//function FPgetgid : gid_t;
//function FPgetegid : gid_t;
//function FPsetuid(uid : uid_t): cint;
//function FPsetgid(gid : gid_t): cint;
//function FPgetgroups(gidsetsize : cint; var grouplist:tgrparr): cint;
//function FPgetpgrp : pid_t;
//function FPsetsid : pid_t;
//Function FPumask(cmask:mode_t):mode_t;
//Function FPlink(existing:pchar;newone:pchar):cint;
//Function FPmkfifo(path:pchar;mode:mode_t):cint;
//Function FPchmod(path:pchar;mode:mode_t):cint;
//Function FPchown(path:pchar;owner:uid_t;group:gid_t):cint;
//Function FPUtime(path:pchar;times:putimbuf):cint;
//Function FPpipe(var fildes : tfildes):cint;
//function FPfcntl(fildes:cint;Cmd:cint;Arg:cint):cint;
//function FPfcntl(fildes:cint;Cmd:cint;var Arg:flock):cint;
//function FPfcntl(fildes:cint;Cmd:cint):cint;
//function FPexecve(path:pchar;argv:ppchar;envp:ppchar):cint;
//function FPexecv(path:pchar;argv:ppchar):cint;
function FPgetrusage(who:cint;var ru : rusage):cint;
//function FPtimes(var buffer : tms):clock_t;

