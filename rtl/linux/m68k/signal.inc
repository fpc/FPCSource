{
    $Id: signal.inc,v 1.1.2.2 2002/10/10 19:31:28 pierre Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller,
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



{ not sure this value is correct for m68k PM }
const
  SI_PAD_SIZE	= ((128 div sizeof(longint)) - 3);


type
  { REMARK: floating point regs are defined as arrays of
    3 longints; I don't know if C does align this to
    16 byte boundaries for each element of the array PM }
  { If C does we might need to define this as
    array from 0 to 3 }

  tfpreg = array[0..2] of longint;

  pfpstate = ^tfpstate;
  tfpstate = record
           pcr,psr,fpiaddr : longint;
	   fpreg : array [0..7] of tfpreg;
  end;

  Size_T = cardinal;

  SigSet  = Longint;
  PSigSet = ^SigSet;

  { as defined in asm_m68k/signal.h }
  Stack_T = Record
    ss_sp : pointer;
    ss_flags : longint;
    ss_size : size_t;
  end;

  { SigContextRec corresponds to the ucontext record
    in linux asm-m68k/ucontext.h include file }
  PSigContextRec = ^SigContextRec;
  SigContextRec = record
    uc_flags : cardinal;
    uc_link : pSigContextRec;
    uc_stack : stack_t; { what's that ?? }
    { fields from 'version' to 'pc'
      correspond to the mcontext struct in asm-m68k/ucontext.h file }
    version : longint; { SigContext version check }
    { 18 general registers }
    d0,d1,d2,d3,d4,d5,d6,d7 : cardinal;
    a0,a1,a2,a3,a4,a5 : cardinal;
    fp,sp,ps,pc : cardinal;
    { fields from 'pcr' to 'fpreg'
      are floating point part }
    pcr,psr,fpiaddr : longint;
    fpreg : array[0..7] of tfpreg; { how is this aligned ?? }
    filler : array[0..79] of cardinal;
    sigmask : SigSet;
  end;


  PSigInfoRec = ^SigInfoRec;
  SigInfoRec = record
    si_signo: longint;
    si_errno: longint;
    si_code: longint;

    case longint of
      0:
        (pad: array[0 .. SI_PAD_SIZE-1] of longint);
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
  { the third argument is only a guess for now,
    asm-m68k/signal.h gives only void * arg type PM }
  TSigAction = procedure(Sig: Longint; SigInfoPtr : PSigInfoRec; SigContextPtr : PSigContextRec);cdecl;


  SigActionRec = packed record
    Handler  : record
      case byte of
        0: (Sh: SignalHandler);
        1: (Sa: TSigAction);
      end;
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
    Sa_restorer : SignalRestorer; { Obsolete - Don't use }
  end;
  PSigActionRec = ^SigActionRec;
(*
Procedure SigAction(Signum:Integer;Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}
Var
  sr : Syscallregs;
begin
  sr.reg2:=Signum;
  sr.reg3:=Longint(act);
  sr.reg4:=Longint(oldact);
  SysCall(Syscall_nr_sigaction,sr);
end; *)

{
  $Log: signal.inc,v $
  Revision 1.1.2.2  2002/10/10 19:31:28  pierre
   * update those files that are unused currently

  Revision 1.1.2.1  2001/07/13 15:05:40  pierre
   + first signal context tries

}
