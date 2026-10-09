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
  SA_USERTRAMP = $100; { SUNOS compat: Do not bounce off kernel's sigtramp }

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
  SIGCHLD   = 20;
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
{$if (defined(CPUi386) or defined(CPUX86_64))}
    SigContextRec = record
       sc_mask      : sigset_t;          { signal mask to restore }
       sc_onstack   : longint;              { sigstack state to restore }

       sc_gs        : longint;              { machine state (struct trapframe): }
       sc_fs        : longint;
       sc_es        : longint;
       sc_ds        : longint;
       sc_edi       : longint;
       sc_esi       : longint;
       sc_ebp       : longint;
       sc_isp       : longint;
       sc_ebx       : longint;
       sc_edx       : longint;
       sc_ecx       : longint;
       sc_eax       : longint;
       sc_trapno    : longint;
       sc_err       : longint;
       sc_eip       : longint;
       sc_cs        : longint;
       sc_efl       : longint;
       sc_esp       : longint;
       sc_ss        : longint;
        {
         * XXX FPU state is 27 * 4 bytes h/w, 1 * 4 bytes s/w (probably not
         * needed here), or that + 16 * 4 bytes for emulators (probably all
         * needed here).  The "spare" bytes are mostly not spare.
         }
       en_cw        : cardinal;     { control word (16bits used) }
       en_sw        : cardinal;     { status word (16bits) }
       en_tw        : cardinal;     { tag word (16bits) }
       en_fip       : cardinal;     { floating point instruction pointer }
       en_fcs       : word;         { floating code segment selector }
       en_opcode    : word;         { opcode last executed (11 bits ) }
       en_foo       : cardinal;     { floating operand offset }
       en_fos       : cardinal;     { floating operand segment selector }
       fpr_acc      : array[0..79] of AnsiChar;
       fpr_ex_sw    : cardinal;
       fpr_pad      : array[0..63] of AnsiChar;
       end;
{$endif def x86}


{$ifdef CPUAARCH64}
        SigContextRec = record
            _dummy : cint;
        end;
{$endif cpuaarch64}

{$ifdef CPUPOWERPC64}
{$i sighndh.inc}
{$endif}

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
                si_pid          : Longint;      { sending process }
                si_uid          : Cardinal;     { sender's ruid }
                si_status       : Longint;      { exit value }
                si_addr         : Pointer;      { faulting instruction }
                si_value        : SigVal;       { signal value }
                si_band         : Cardinal;     { band event for SIGPOLL }
                __spare         : array[0..6] of Longint; { gimme some slack }
                end;
  TSigInfo = SigInfo_t;
  TSigInfo_t = TSigInfo;



  SignalHandler   = Procedure(Sig : Longint);cdecl;
  TSignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
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


{$ifndef CPUPOWERPC64}
  pstack_t = ^stack_t;
  stack_t = record
    ss_sp: PAnsiChar;                       {* signal stack base *}
    ss_size: size_t;                    {* signal stack length *}
    ss_flags: cInt;                     {* SS_DISABLE and/or SS_ONSTACK *}
  end;
  TStack = stack_t;
  PStack = pstack_t;
{$endif CPUPOWERPC64}

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

{ si_code values when si_signo = SIGILL }
        ILL_ILLOPC      =1;     { illegal opcode }
        ILL_ILLOPN      =2;     { illegal operand }
        ILL_ILLADR      =3;     { illegal addressing mode }
        ILL_ILLTRP      =4;     { illegal trap }
        ILL_PRVOPC      =5;     { privileged opcode }
        ILL_PRVREG      =6;     { privileged register }
        ILL_COPROC      =7;     { coprocessor error }
        ILL_BADSTK      =8;     { internal stack error }

{ si_code values when si_signo = SIGSEGV }
        SEGV_MAPERR     =1;     { address not mapped to object }
        SEGV_ACCERR     =2;     { invalid permissions for mapped object }

{ si_code values when si_signo = SIGBUS }
        BUS_ADRALN      =1;     { invalid address alignment }
        BUS_ADRERR      =2;     { nonexistent physical address }
        BUS_OBJERR      =3;     { object-specific hardware error }

{ si_code values when si_signo = SIGTRAP }
        TRAP_BRKPT      =1;     { process breakpoint }
        TRAP_TRACE      =2;     { process trace trap }

{ si_code values when si_signo = SIGCHLD }
        CLD_EXITED      =1;     { child has exited }
        CLD_KILLED      =2;     { child has terminated abnormally, no core file }
        CLD_DUMPED      =3;     { child has terminated abnormally, core file }
        CLD_TRAPPED     =4;     { traced child has trapped }
        CLD_STOPPED     =5;     { child has stopped }
        CLD_CONTINUED   =6;     { stopped child has continued }

{ si_code values when si_signo = SIGPOLL }
        POLL_IN         =1;     { data input available }
        POLL_OUT        =2;     { output buffers available }
        POLL_MSG        =3;     { input message available }
        POLL_ERR        =4;     { i/o error }
        POLL_PRI        =5;     { high priority input available }
        POLL_HUP        =6;     { device disconnected }

{ si_code values for any signal }
        SI_NOINFO       =0;        { no signal info besides si_signo }
        SI_USER         =$10001;   { signal sent by kill() }
        SI_QUEUE        =$10002;   { signal sent by sigqueue() }
        SI_TIMER        =$10003;   { timer expiration }
        SI_ASYNCIO      =$10004;   { asynchronous i/o request completion }
        SI_MESGQ        =$10005;   { message arrival on an empty message queue }
        SI_KERNEL       =$10006;   { signal sent by the kernel }
        SI_LWP          =$10007;   { signal sent by thr_kill() }



