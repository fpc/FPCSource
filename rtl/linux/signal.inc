{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

{********************
      Signal
********************}

Const
  { For sending a signal }
  SA_NOCLDSTOP = 1;
  SA_SHIRQ     = $04000000;
  SA_STACK     = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NOMASK    = $40000000;
  SA_ONESHOT   = $80000000;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;

  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

{$ifdef cpusparc}
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
  SIGPOLL    = SIGIO;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGLOST    = 29;
  SIGPWR     = SIGLOST;
  SIGUSR1    = 30;
  SIGUSR2    = 31;
{$else cpusparc}
  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGBUS     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGUSR1    = 10;
  SIGSEGV    = 11;
  SIGUSR2    = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTerm    = 15;
  SIGSTKFLT  = 16;
  SIGCHLD    = 17;
  SIGCONT    = 18;
  SIGSTOP    = 19;
  SIGTSTP    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGURG     = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGIO      = 29;
  SIGPOLL    = SIGIO;
  SIGPWR     = 30;
  SIGUNUSED  = 31;
{$endif cpusparc}


const
  SI_PAD_SIZE   = ((128 div sizeof(longint)) - 3);

type
  tfpreg = record
          significand: array[0..3] of word;
          exponent: word;
  end;

  pfpstate = ^tfpstate;
  tfpstate = record
           cw, sw, tag, ipoff, cssel, dataoff, datasel: cardinal;
           st: array[0..7] of tfpreg;
           status: cardinal;
  end;

  SigSet  =  array[0..wordsinsigset-1] of cint;
  sigset_t= SigSet;
  PSigSet = ^SigSet;
  psigset_t=psigset;
  TSigSet = SigSet;


{$ifdef cpui386}
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    gs, __gsh: word;
    fs, __fsh: word;
    es, __esh: word;
    ds, __dsh: word;
    edi: cardinal;
    esi: cardinal;
    ebp: cardinal;
    esp: cardinal;
    ebx: cardinal;
    edx: cardinal;
    ecx: cardinal;
    eax: cardinal;
    trapno: cardinal;
    err: cardinal;
    eip: cardinal;
    cs, __csh: word;
    eflags: cardinal;
    esp_at_signal: cardinal;
    ss, __ssh: word;
    fpstate: pfpstate;
    oldmask: cardinal;
    cr2: cardinal;
  end;
{$endif cpui386}


{$Ifdef cpum68k}
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    { dummy for now PM }
  end;
{$endif cpum68k}


{$ifdef cpupowerpc}
  { from include/ppc/ptrace.h }
  pptregs = ^tptregs;
  tptregs = record
    gpr: array[0..31] of cardinal;
    nip: cardinal;
    msr: cardinal;
    orig_gpr3: cardinal; { Used for restarting system calls }
    ctr: cardinal;
    link: cardinal;
    xer: cardinal;
    ccr: cardinal;
    mq: cardinal;        { 601 only (not used at present)  }
                         { Used on APUS to hold IPL value. }
    trap: cardinal;      { Reason for being here }
    dar: cardinal;       { Fault registers }
    dsisr: cardinal;
    result: cardinal;    { Result of a system call }
  end;

  { from include/asm/ppc/siginfo.h }
     psiginfo = ^tsiginfo;
     tsiginfo = record
          si_signo : longint;
          si_errno : longint;
          si_code : longint;
          _sifields : record
              case longint of
                 0 : ( _pad : array[0..(SI_PAD_SIZE)-1] of longint );
                 1 : ( _kill : record
                      _pid : pid_t;
                      _uid : uid_t;
                   end );
                 2 : ( _timer : record
                      _timer1 : dword;
                      _timer2 : dword;
                   end );
                 3 : ( _rt : record
                      _pid : pid_t;
                      _uid : uid_t;
                      _sigval : pointer;
                   end );
                 4 : ( _sigchld : record
                      _pid : pid_t;
                      _uid : uid_t;
                      _status : longint;
                      _utime : clock_t;
                      _stime : clock_t;
                   end );
                 5 : ( _sigfault : record
                      _addr : pointer;
                   end );
                 6 : ( _sigpoll : record
                      _band : longint;
                      _fd : longint;
                   end );
              end;
       end;


  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: longint;
    ss_size: size_t;
  end;

  { from include/asm-ppc/sigcontext.h }
  tsigcontext_struct = record
    _unused: array[0..3] of dword;
    signal: longint;
    handler: dword;
    oldmask: dword;
    pt_regs: pptregs;
  end;

  { from include/asm-ppc/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : dword;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_mcontext : tsigcontext_struct;
    uc_sigmask : sigset_t;
  end;


  { from arch/ppc/kernel/signal.c, the type of the actual parameter passed }
  { to the sigaction handler                                               }
  t_rt_sigframe = record
    _unused: array[0..1] of cardinal;
    pinfo: psiginfo;
    puc: pointer;
    siginfo: tsiginfo;
    uc: tucontext;
  end;

  PSigContextRec = ^SigContextRec;
  SigContextRec = tsigcontext_struct;
{$endif cpupowerpc}


{$ifdef cpusparc}
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    { dummy for now PM }
  end;
{$endif cpusparc}

{$ifdef cpux86_64}

  p_fpstate = ^_fpstate;
  _fpstate = packed record
    cwd,
    swd,
    twd,    // Note this is not the same as the 32bit/x87/FSAVE twd
    fop : word;
    rip,
    rdp : qword;
    mxcsr,
    mxcsr_mask : dword;
    st_space : array[0..31] of dword;  // 8*16 bytes for each FP-reg
    xmm_space : array[0..63] of dword; // 16*16 bytes for each XMM-reg
    reserved2 : array[0..23] of dword;
  end;

  PSigContextRec = ^SigContextRec;
  SigContextRec = packed record
    __pad00 : array[0..4] of qword;
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    rdi,
    rsi,
    rbp,
    rbx,
    rdx,
    rax,
    rcx,
    rsp,
    rip,
    eflags : qword;
    cs,
    gs,
    fs,
    __pad0 : word;
    err,
    trapno,
    oldmask,
    cr2 : qword;
    fpstate : p_fpstate;       // zero when no FPU context */
    reserved1 : array[0..7] of qword;
  end;
{$endif cpux86_64}

{$ifdef cpuarm}
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    trap_no : dword;
    error_code : dword;
    oldmask : dword;
    arm_r0 : dword;
    arm_r1 : dword;
    arm_r2 : dword;
    arm_r3 : dword;
    arm_r4 : dword;
    arm_r5 : dword;
    arm_r6 : dword;
    arm_r7 : dword;
    arm_r8 : dword;
    arm_r9 : dword;
    arm_r10 : dword;
    arm_fp : dword;
    arm_ip : dword;
    arm_sp : dword;
    arm_lr : dword;
    arm_pc : dword;
    arm_cpsr : dword;
    fault_address : dword;
  end;

  { from include/asm-ppc/signal.h }
  stack_t = record
    ss_sp: pointer;
    ss_flags: longint;
    ss_size: size_t;
  end;

  { from include/asm-arm/ucontext.h }
  pucontext = ^tucontext;
  tucontext = record
    uc_flags : dword;
    uc_link : pucontext;
    uc_stack : stack_t;
    uc_mcontext : SigContextRec;
    uc_sigmask : sigset_t;
  end;
{$endif cpuarm}

  PSigInfoRec = ^SigInfoRec;
  SigInfoRec = record
    si_signo: longint;
    si_errno: longint;
    si_code: longint;

    case longint of
      0:
        (pad: array[0..SI_PAD_SIZE-1] of longint);
      1: { kill }
        ( kill: record
            pid: longint;  { sender's pid }
            uid : longint; { sender's uid }
          end );
      2: { POSIX.1b timers }
        ( timer : record
            timer1 : cardinal;
            timer2 : cardinal;
           end );
      3: { POSIX.1b signals }
        ( rt : record
            pid : longint;    { sender's pid }
            uid : longint;    { sender's uid }
            sigval : longint;
         end );
      4: { SIGCHLD }
        ( sigchld : record
          pid : longint;    { which child }
          uid : longint;    { sender's uid }
          status : longint; { exit code }
          utime : timeval;
          stime : timeval;
         end );
      5: { SIGILL, SIGFPE, SIGSEGV, SIGBUS }
        ( sigfault : record
            addr : pointer;{ faulting insn/memory ref. }
          end );
      6:
        ( sigpoll : record
            band : longint; { POLL_IN, POLL_OUT, POLL_MSG }
            fd : longint;
          end );
  end;

  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  TSigAction = procedure(Sig: Longint; SigContext: SigContextRec);cdecl;


{$ifdef CPUARM}
{$define NEWSIGNAL}
{$endif CPUARM}

{$ifdef CPUx86_64}
{$define NEWSIGNAL}
{$endif CPUx86_64}

  SigActionRec = packed record  // this is temporary for the migration
   {$ifdef posixworkaround}
    sa_handler : signalhandler;
   {$else}
    Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;
   {$endif}
   {$ifdef NEWSIGNAL}
    Sa_Flags    : cuint;
    Sa_restorer : SignalRestorer; { Obsolete - Don't use }
    Sa_Mask     : SigSet;
   {$else NEWSIGNAL}
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
    Sa_restorer : SignalRestorer; { Obsolete - Don't use }
   {$endif NEWSIGNAL}
  end;
  TSigActionRec = SigActionRec;
  PSigActionRec = ^SigActionRec;

{
  $Log$
  Revision 1.20  2004-05-27 23:15:43  peter
    * sparc signals added between $ifdef cpusparc

  Revision 1.19  2004/05/01 15:59:17  florian
    * x86_64 exception handling fixed

  Revision 1.18  2004/04/27 20:47:00  florian
    * tried to fix x86-64 signal handling

  Revision 1.17  2004/03/27 14:35:13  florian
    * structs for arm adapted

  Revision 1.16  2004/02/05 01:16:12  florian
    + completed x86-64/linux system unit

  Revision 1.15  2004/01/01 16:28:16  jonas
    * fixed signal handling

  Revision 1.14  2003/11/21 00:40:06  florian
    * some arm issues fixed

  Revision 1.13  2003/11/02 14:53:06  jonas
    + sighand and associated record definitions for ppc. Untested.

  Revision 1.12  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.11  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.10  2003/08/21 22:24:52  olle
    - removed parameter from fpc_iocheck

  Revision 1.9  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.8  2002/12/18 16:43:26  marco
   * new unix rtl, linux part.....

  Revision 1.7  2002/11/12 14:51:44  marco
   * signal.

  Revision 1.6  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/07/28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}

