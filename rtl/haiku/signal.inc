{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Const   { For sending a signal }

  SA_NOCLDSTOP = $01;
  SA_NOCLDWAIT = $02;
  SA_RESETHAND = $04;
  SA_NODEFER   = $08;
  SA_RESTART   = $10;
  SA_ONSTACK   = $20;
  SA_SIGINFO   = $40;
  SA_NOMASK    = SA_NODEFER;
  SA_STACK     = SA_ONSTACK;
  SA_ONESHOT   = SA_RESETHAND;

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 3;

{ values for ss_flags }
  SS_ONSTACK = $1;
  SS_DISABLE = $2;

  MINSIGSTKSZ = 4096;
  SIGSTKSZ = 16384;

{Haiku Checked}
{
   The numbering of signals for BeOS attempts to maintain 
   some consistency with UN*X conventions so that things 
   like "kill -9" do what you expect.
}

  SIG_DFL  =  0;
  SIG_IGN  =  1;
  SIG_ERR  = -1;
  SIG_HOLD =  3;

  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGCHLD    = 5;
  SIGABRT    = 6;
  SIGPIPE    = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGSTOP    = 10;
  SIGSEGV    = 11;
  SIGCONT    = 12;
  SIGTSTP    = 13;
  SIGALRM    = 14;
  SIGTERM    = 15;
  SIGTTIN    = 16;
  SIGTTOU    = 17;
  SIGUSR1    = 18;
  SIGUSR2    = 19;
  SIGWINCH   = 20;
  SIGKILLTHR = 21;
  SIGTRAP    = 22;
  SIGPOLL    = 23;
  SIGPROF    = 24;
  SIGSYS     = 25;
  SIGURG     = 26;
  SIGVTALRM  = 27;
  SIGXCPU    = 28;
  SIGXFSZ    = 29;
  SIGBUS     = 30;
  SIGRESERVED1 = 31;
  SIGRESERVED2 = 32;


{ Include BeOS/Haiku specific vregs struct, which is architecture dependent
  and maps directly as mcontext_t }
{$include sig_cpu.inc}

{$packrecords C}
type
  mcontext_t = vregs;
  Pvregs = ^vregs;

  pstack_t = ^stack_t;
  stack_t = record
    ss_sp: pointer;                       {* signal stack base *}
    ss_size: size_t;                    {* signal stack length *}
    ss_flags: cint;                     {* SS_DISABLE and/or SS_ONSTACK *}
  end;
  TStack = stack_t;
  PStack = pstack_t;

  sigset_t = array[0..wordsinsigset-1] of dword;

  PSigContext = ^SigContextRec;
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    uc_link: PSigContextRec;
    uc_sigmask: sigset_t;
    uc_stack: stack_t;
    uc_mcontext: mcontext_t;
  end;

  Sigval = record
    case boolean of
      { Members as suggested by Annex C of POSIX 1003.1b. }
      false : (sigval_int : Longint);
      true  : (sigval_ptr : Pointer);
  end;

  PSigInfo   = ^SigInfo_t;
  PSigInfo_t = ^SigInfo_t;
  SigInfo_t = record
    si_signo: cint;     { signal number }
    si_code: cint;      { signal code }
    si_errno: cint;     { if non zero, an error number associated with this signal }
    si_pid: pid_t;      { sending process }
    si_uid: uid_t;      { sender's ruid }
    si_addr: Pointer;   { faulting instruction }
    si_status: cint;    { exit value }
    si_band: clong;     { band event for SIGPOLL }
    si_value: SigVal;   { signal value }
  end;
  TSigInfo = SigInfo_t;
  TSigInfo_t = TSigInfo;

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  SigActionHandler = procedure(Sig: Longint; SigInfo: PSigInfo; uContext : PSigContext);cdecl;

  Sigset=sigset_t;
  TSigset=sigset_t;
  PSigSet = ^SigSet;
  psigset_t=psigset;

  PSigActionRec = ^SigActionRec;
  SigActionRec = record
    sa_handler : SigActionHandler;
    sa_Mask    : SigSet;
    sa_Flags   : Longint;
    sa_userdata: pointer;
  end;


{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
