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
  SI_MAXSZ = 128;
  SI_PAD_SIZE = ((SI_MAXSZ div sizeof(longint)) - 3);

{
 * The sequence of the fields/registers in struct sigcontext should match
 * those in mcontext_t.
 }

type sigset_t = array [0..0] of cuint32 {array[0..3] of cardinal};

    PSigContextRec = ^SigContextRec;
    psigcontext = ^sigcontextrec;
{$ifdef cpui386}
    PFpuState = ^TFpuState;
{$endif def cpui386}
{$ifdef cpux86_64}
    PFpu64State = ^TFpu64State;
{$endif def cpux86_64}
    SigContextRec = record
{$ifdef cpui386}
(*     sc_mask      : sigset_t;          { signal mask to restore }
       sc_onstack   : longint;              { sigstack state to restore }
       I did not find those fields in OpenBSD
       /usr/iclude/i386/signal.h header *)
       sc_gs        : longint;              { machine state (struct trapframe): }
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
       sc_mask      : longint;
       sc_trapno    : longint;
       sc_err       : longint;
       sc_fpustate  : pfpustate;
{$endif def cpui386}
{$ifdef cpux86_64}
	sc_rdi : clong;
	sc_rsi : clong;
        sc_rdx : clong;
	sc_rcx : clong;
	sc_r8  : clong;
	sc_r9  : clong;
	sc_r10 : clong;
	sc_r11 : clong;
	sc_r12 : clong;
	sc_r13 : clong;
	sc_r14 : clong;
	sc_r15 : clong;
	sc_rbp : clong;
	sc_rbx : clong;
	sc_rax : clong;
	sc_gs  : clong;
	sc_fs  : clong;
	sc_es  : clong;
	sc_ds  : clong;
	sc_trapno : clong;
	sc_err : clong;
	sc_rip : clong;
	sc_cs  : clong;
	sc_rflags : clong;
	sc_rsp : clong;
	sc_ss  : clong;
	sc_fpstate : PFpu64State;
	sc_onstack : cint;
	sc_mask : cint;
{$endif def cpux86_64}
     end;
{$ifdef cpui386}
     TFpuX87State = record
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
       fpr_acc      : array[0..79] of char;
       fpr_ex_sw    : cardinal;     { status word of last exception }
       fpr_ex_tw    : cardinal;     { tag word of last exception }
       fpr_pad      : array[0..63] of char;
       end;
    TFpuXMMState = packed record
	fx_fcw : cuint16;
        fx_fsw : cuint16;
        fx_ftw : cuint8;
        fx_unused1 : cuint8;
        fx_fop : cuint16;
        fx_rip : cuint64;
        fx_rdp : cuint64;
        fx_mxcsr : cuint32;
        fx_mxcsr_mask : cuint32;
        fx_st : array[0..7] of array [0..1] of cuint64;
        fx_xmm : array [0..15] of array [0..1] of cuint64;
        fx_unusued3 : array [0..95] of cuint8;
    end;
    TFpuState = record
      case integer of
       0 : (X87State : TFpuX87State);
       1 : (XMMState : TFpuXMMState);
    end; 
{$endif def cpui386}

(*  From /usr/include/amd64/fpu.h header 
/*
 * amd64 only uses the extended save/restore format used
 * by fxsave/fsrestore, to always deal with the SSE registers,
 * which are part of the ABI to pass floating point values.
 * Must be stored in memory on a 16-byte boundary.
 */

struct fxsave64 {
	u_int16_t  fx_fcw;
	u_int16_t  fx_fsw;
	u_int8_t   fx_ftw;
	u_int8_t   fx_unused1;
	u_int16_t  fx_fop;
	u_int64_t  fx_rip;
	u_int64_t  fx_rdp;
	u_int32_t  fx_mxcsr;
	u_int32_t  fx_mxcsr_mask;
	u_int64_t  fx_st[8][2];   /* 8 normal FP regs */
	u_int64_t  fx_xmm[16][2]; /* 16 SSE2 registers */
	u_int8_t   fx_unused3[96];
} __packed;

struct savefpu {
	struct fxsave64 fp_fxsave;	/* see above */
	u_int16_t fp_ex_sw;		/* saved status from last exception */
	u_int16_t fp_ex_tw;		/* saved tag from last exception */
}; *)
{$ifdef cpux86_64}
    TFpu64State = packed record
	fx_fcw : cuint16;
        fx_fsw : cuint16;
        fx_ftw : cuint8;
        fx_unused1 : cuint8;
        fx_fop : cuint16;
        fx_rip : cuint64;
        fx_rdp : cuint64;
        fx_mxcsr : cuint32;
        fx_mxcsr_mask : cuint32;
        fx_st : array[0..7] of array [0..1] of cuint64;
        fx_xmm : array [0..15] of array [0..1] of cuint64;
        fx_unusued3 : array [0..95] of cuint8;
    end;
{$endif def cpux86_64}

(* From /usr/include/amd64/signal.h 
  struct sigcontext {
	/* plain match trapframe */
   Sig64Context  = Record
	long	sc_rdi;
	long	sc_rsi;
	long	sc_rdx;
	long	sc_rcx;
	long	sc_r8;
	long	sc_r9;
	long	sc_r10;
	long	sc_r11;
	long	sc_r12;
	long	sc_r13;
	long	sc_r14;
	long	sc_r15;
	long	sc_rbp;
	long	sc_rbx;
	long	sc_rax;
	long	sc_gs;
	long	sc_fs;
	long	sc_es;
	long	sc_ds;
	long	sc_trapno;
	long	sc_err;
	long	sc_rip;
	long	sc_cs;
	long	sc_rflags;
	long	sc_rsp;
	long	sc_ss;

	struct fxsave64 *sc_fpstate;
	int	sc_onstack;
	int	sc_mask;
}; *)
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
        {
         * Cause of signal, one of the SI_ macros or signal-specific
         * values, i.e. one of the FPE_... values for SIGFPE. This
         * value is equivalent to the second argument to an old-style
         * FreeBSD signal handler.
         }
                si_code,                        { signal code }
                si_errno : cint32;              { errno association }
                case integer of
                0 : (
                si_pid          : Longint;      { sending process }
                si_uid          : Cardinal;     { sender's ruid }
                si_status       : Longint;      { exit value }
                si_addr         : Pointer;      { faulting instruction }
                si_value        : SigVal;       { signal value }
                si_band         : Cardinal;     { band event for SIGPOLL }
                __spare         : array[0..6] of Longint;); { gimme some slack }
                1 : ( fault_addr : pointer;
                      fault_trapno : cint; );
                2 : (pad : array [0..SI_PAD_SIZE] of cuint32;);
                end;
  TSigInfo = SigInfo_t;
  TSigInfo_t = TSigInfo;


  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  sigActionHandler = procedure(Sig: Longint; sininfo:psiginfo; SigContext: PSigContext);cdecl;



{$ifdef powerpc}
  TSigaction=  procedure(Sig: Longint); cdecl;
{$else}
{$define BSDHandler}
{$ifdef BSDHandler}
  TSigAction = procedure(Sig: Longint; code:longint;var SigContext: SigContextRec);cdecl;
{$else}
  TSigAction = procedure(Sig: Longint; var sininfo:tsiginfo_t;var SigContext: SigContextRec);cdecl;
{$endif}
{$endif}

  Sigset=sigset_t;
  TSigset=sigset_t;
  PSigSet = ^SigSet;

  PSigActionRec = ^SigActionRec;
  SigActionRec = packed record
//    Handler  : record
    sa_handler  : sigActionHandler;
//    sa_handler : TSigAction;
//      case byte of
//        0: (Sh: SignalHandler);
//        1: (Sa: TSigAction);
//      end;
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
  end;

{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}


