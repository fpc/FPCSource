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

  SA_NOCLDSTOP = 8;
  SA_ONSTACK   = $001; { take signal on signal stack }
  SA_RESTART   = $002; { restart system call on signal return }
  SA_RESETHAND = $004; { reset to SIG_DFL when taking signal }
  SA_NODEFER   = $010; { don't mask the signal we're delivering }
  SA_NOCLDWAIT = $020; { don't keep zombies around }
  SA_SIGINFO   = $040; { signal handler with SA_SIGINFO args }

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 3;

{BSD Checked}
  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGEMT     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGBUS     = 10;
  SIGSEGV    = 11;
  SIGSYS     = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTERM    = 15;
  SIGURG     = 16;
  SIGSTOP    = 17;
  SIGTSTP    = 18;
  SIGCONT    = 19;
  SIGCHLD    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGIO      = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGINFO    = 29;
  SIGUSR1    = 30;
  SIGUSR2    = 31;


{$packrecords C}
const
  SI_PAD_SIZE   = ((128/sizeof(longint)) - 3);

{
 * The sequence of the fields/registers in struct sigcontext should match
 * those in mcontext_t.
 }

type sigset_t = array[0..3] of Longint;

    psigcontext = ^sigcontextrec;
    PSigContextRec = ^SigContextRec;
    SigContextRec = record
       sc_mask      : sigset_t;          { signal mask to restore }
       sc_onstack   : cint64;              { sigstack state to restore }

       sc_rdi       : cint64;              { machine state (struct trapframe): }
       sc_rsi       : cint64;
       sc_rdx       : cint64;
       sc_rcx       : cint64;
       sc_r8        : cint64;
       sc_r9        : cint64;
       sc_rax       : cint64;
       sc_rbx       : cint64;
       sc_rbp       : cint64;
       sc_r10       : cint64;
       sc_r11       : cint64;
       sc_r12       : cint64;
       sc_r13       : cint64;
       sc_r14       : cint64;
       sc_r15       : cint64;
       sc_xflags    : cint64;
       sc_trapno    : cint64;
       sc_addr      : cint64;
       sc_flags     : cint64;
       sc_err       : cint64;
       sc_rip       : cint64;
       sc_cs        : cint64;
       sc_rflags    : cint64;
       sc_rsp       : cint64;
       sc_ss        : cint64;
       sc_len       : cuint32;
       sc_fpformat  : cuint32;
       sc_ownedfp   : cuint32;
       sc_reserved  : cuint32;
       sc_unused    : array[0..7] of cuint32;
       sc_pregs     : array[0..255] of cuint32;
    end;



  Sigval = Record
            Case Boolean OF
        { Members as suggested by Annex C of POSIX 1003.1b. }
                false : (sigval_int : Longint);
                True  : (sigval_ptr : Pointer);
            End;


  PSigInfo   = ^SigInfo_t;
  PSigInfo_t = ^SigInfo_t;
  SigInfo_t = record
                si_signo,                       { signal number }
                si_errno,                       { errno association }
        {
         * Cause of signal, one of the SI_ macros or signal-specific
         * values, i.e. one of the FPE_... values for SIGFPE. This
         * value is equivalent to the second argument to an old-style
         * FreeBSD signal handler.
         }
                si_code,                        { signal code }
                si_pid          : cint32;       { sending process }
                si_uid          : cuint32;      { sender's ruid }
                si_status       : cint32;       { exit value }
                si_addr         : Pointer;      { faulting instruction }
                si_value        : SigVal;       { signal value }
                si_band         : cint32;       { band event for SIGPOLL }
                __spare         : array[0..6] of cint32; { gimme some slack }
                end;
  TSigInfo = SigInfo_t;
  TSigInfo_t = TSigInfo;



  SignalHandler    = Procedure(Sig : Longint);cdecl;
  TSignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler   = ^SignalHandler;
  SignalRestorer   = Procedure;cdecl;
  PSignalRestorer  = ^SignalRestorer;
  sigActionHandler = procedure(Sig: Longint; sininfo:psiginfo; SigContext: PSigContext);cdecl;

  TSigset=sigset_t;
  sigset=tsigset;
  PSigSet = ^TSigSet;

  SigActionRec = packed record
{     Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;}
    sa_handler  : sigActionHandler;
    Sa_Flags    : Longint;
    Sa_Mask     : TSigSet;
  end;
  PSigActionRec = ^SigActionRec;
  

  pstack_t = ^stack_t;
  stack_t = record
    ss_sp   : pChar;                    {* signal stack base *}
    ss_size : size_t;                   {* signal stack length *}
    ss_flags: cint32;                   {* SS_DISABLE and/or SS_ONSTACK *}
  end;
  TStack = stack_t;
  PStack = pstack_t;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
const
        FPE_INTOVF      =1;     { integer overflow }
        FPE_INTDIV      =2;     { integer divide by zero }
        FPE_FLTDIV      =3;     { floating point divide by zero }
        FPE_FLTOVF      =4;     { floating point overflow }
        FPE_FLTUND      =5;     { floating point underflow }
        FPE_FLTRES      =6;     { floating point inexact result }
        FPE_FLTINV      =7;     { invalid floating point operation }
        FPE_FLTSUB      =8;     { subscript out of range }
