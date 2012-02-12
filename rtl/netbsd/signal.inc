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

{ from NetBSD /usr/include/sys/siginfo.h }
const
  SI_PAD_SIZE   = 128;

type

  tsigval = record
      sival_int : cint;
      sival_ptr : pointer;
    end;

  tsig_rt = record
      _uid : tuid; { defined in ptypes.inc }
      _value : tsigval;
    end;

  tsig_child = record
      _pid : tpid;
      _status : cint;
      _utime : tclock;
      _stime : tclock;
    end;

  tsig_fault = record
      _addr : pointer;
      _trap : cint;
    end;

  tsig_poll = record
     _band : clong;
     _fd : cint;
    end;

  ksiginfo = record
       si_signo : cint;
       si_code  : cint;
       si_errno : cint;
{$ifdef cpu64}
       si_pad   : cint;
{$endif cpu64}
       _reason : record
         case longint of
          0 : (_rt : tsig_rt);
          1 : (_child : tsig_child);
          2 : (_fault : tsig_fault);
          3 : (_poll : tsig_poll);
          { not yet filled }
        end;
   end;

  psiginfo = ^tsiginfo;
  tsiginfo = record
          case longint of
              0 : ( _pad : array[0..(SI_PAD_SIZE)-1] of char );
              1 : ( _info : ksiginfo);
          { end; }
    end;

{
 * The sequence of the fields/registers in struct sigcontext should match
 * those in mcontext_t.
 }

type sigset_t = array[0..3] of cardinal;

    PSigContextRec = ^SigContextRec;
    PSigContext = ^SigContextRec;
{
struct sigcontext {
	int	sc_gs;
	int	sc_fs;
	int	sc_es;
	int	sc_ds;
	int	sc_edi;
	int	sc_esi;
	int	sc_ebp;
	int	sc_ebx;
	int	sc_edx;
	int	sc_ecx;
	int	sc_eax;
	/* XXX */
	int	sc_eip;
	int	sc_cs;
	int	sc_eflags;
	int	sc_esp;
	int	sc_ss;

	int	sc_onstack;		/* sigstack state to restore */
	int	__sc_mask13;		/* signal mask to restore (old style) */

	int	sc_trapno;		/* XXX should be above */
	int	sc_err;

	sigset_t sc_mask;		/* signal mask to restore (new style) */
};
}
    SigContextRec = record
       sc_gs        : longint;   { machine state (struct trapframe): }
       sc_fs        : longint;
       sc_es        : longint;
       sc_ds        : longint;
       sc_edi       : longint;
       sc_esi       : longint;
       sc_ebp       : longint;
       sc_ebx       : longint;
       sc_edx       : longint;
       sc_ecx       : longint;
       sc_eax       : longint;
       sc_eip       : longint;
       sc_cs        : longint;
       sc_efl       : longint;
       sc_esp       : longint;
       sc_ss        : longint;
       sc_onstack   : longint;
       __sc_mask13  : longint;
       sc_trapno    : longint;
       sc_err       : longint;
       sc_mask      : sigset_t;
 
(*           {
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
       fpr_acc      : array[0..79] of char;
       fpr_ex_sw    : cardinal;
       fpr_pad      : array[0..63] of char; *)
       end;

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;

  sigActionHandler = procedure(Sig: Longint; sininfo:psiginfo; SigContext: PSigContext);cdecl;


  Sigset=sigset_t;
  TSigset=sigset_t;
  PSigSet = ^SigSet;

  SigActionRec = packed record
//    Handler  : record
    sa_handler : SigActionHandler;
//      case byte of
//        0: (Sh: SignalHandler);
//        1: (Sa: TSigAction);
//      end;
    Sa_Flags    : Longint;
    Sa_Mask     : SigSet;
  end;

  PSigActionRec = ^SigActionRec;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}


