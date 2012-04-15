{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Free Pascal development team

    This file implements all the types/constants related
    to signals for AIX.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  {************************ signals *****************************}
  { more can be provided. Herein are only included the required  }
  { values.                                                      }
  {**************************************************************}
  SIGHUP     =  1;     { Hangup                         }
  SIGINT     =  2;     { Interactive attention signal   }
  SIGQUIT    =  3;     { Interactive termination signal }
  SIGILL     =  4;     { Illegal instruction            }
  SIGABRT    =  6;     { abnormal termination           }
  SIGFPE     =  8;     { illegal arithmetic operation   }
  SIGKILL    =  9;     { Kill, cannot be caught         }
  SIGBUS     = 10;     { Access to undefined memory        }
  SIGSEGV    = 11;     { Detection of invalid memory reference }
  SIGPIPE    = 13;     { Broken pipe signal             }
  SIGALRM    = 14;     { alarm clock (used with alarm() }
  SIGTERM    = 15;     { Termination request           }
  SIGUSR1    = 30;
  SIGUSR2    = 31;
  SIGSTOP    = 17;     { Stop signal. cannot be cuaght     }
  SIGSTP     = 18;     { Interactive stop signal           }
  SIGCONT    = 19;     { Continue if stopped               }
  SIGCHLD    = 20;     { Child process terminated / stopped }
  SIGTSTP    = SIGSTP; { AIX name for SIGSTP               }
  SIGTTIN    = 21;     { Background read from TTY          }
  SIGTTOU    = 22;     { Background write to TTY           }

  { AIX specific signals                               }

  SIGTRAP    =  5;      { trace trap (not reset when caught) }
  SIGIOT     = SIGABRT; { IOT instruction }
  SIGLOST    = SIGIOT;  { resource lost (eg, record-lock lost) }
  SIGEMT     =  7;      { EMT instruction }
  SIGSYS     = 12;      { bad argument to system call }
  SIGCLD     = SIGCHLD; { child status change }
  SIGURG     = 16;      { (+) urgent contition on I/O channel  }
  SIGPOLL    = 23;      { pollable event occured }
  SIGXCPU    = 24;      { exceeded cpu limit }
  SIGXFSZ    = 25;
  SIGMSG     = 27;
  SIGWINCH   = 28;      { window size change }
  SIGPWR     = 29;      { power-fail restart }
  SIGIO      = SIGPOLL; { socket I/O possible (SIGPOLL alias) }
  SIGAIO     = SIGIO;
  SIGPTY     = SIGIO;
  SIGPROF    = 32;      { profiling timer expired }
  SIGVTALRM  = 34;      { virtual timer expired }
  SIGMIGRATE = 35;      { exceeded file size limit }
  SIGPRE     = 36;
  SIGVIRT    = 37;
  SIGALARM1  = 38;
  SIGWAITING = 39;      { process's lwps are blocked }
  SIGRECONFIG = 58;
  SIGCPUFAIL = 59;
  SIGKAP     = 60;
  SIGGRANT   = SIGKAP;
  SIGRETRACT = 61;
  SIGSOUND   = 62;
  SIGSAK     = 63;

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 3;

  SIG_DFL = pointer(0) ;
  SIG_IGN = pointer(1) ;
  SIG_ERR = pointer(-1) ;

  { definitions for the sa_flags field }
  SA_ONSTACK   = $00000001;
  SA_RESETHAND = $00000002;
  SA_RESTART   = $00000008;
  SA_SIGINFO   = $00000100;
  SA_NODEFER   = $00000200;
  SA_NOCLDWAIT = $00000400;

  SI_USER = 0;
  SI_UNDEFINED = 8;
  SI_EMPTY = 9;
{ SIGBUS }
  BUS_ADRALN = 1;
  BUS_ADRERR = 2;
  BUS_OBJERR = 3;
  BUS_UEGARD = 4;
{ SIGCHLD }
  CLD_EXITED = 10;
  CLD_KILLED = 11;
  CLD_DUMPED = 12;
  CLD_TRAPPED = 13;
  CLD_STOPPED = 14;
  CLD_CONTINUED = 15;
{ SIGFPE }
  FPE_INTDIV = 20;
  FPE_INTOVF = 21;
  FPE_FLTDIV = 22;
  FPE_FLTOVF = 23;
  FPE_FLTUND = 24;
  FPE_FLTRES = 25;
  FPE_FLTINV = 26;
  FPE_FLTSUB = 27;
{ SIGILL }
  ILL_ILLOPC = 30;
  ILL_ILLOPN = 31;
  ILL_ILLADR = 32;
  ILL_ILLTRP = 33;
  ILL_PRVOPC = 34;
  ILL_PRVREG = 35;
  ILL_COPROC = 36;
  ILL_BADSTK = 37;
{ SIGPOLL }
  POLL_IN = 40;
  POLL_OUT = 41;
  POLL_MSG = -3;
  POLL_ERR = 43;
  POLL_PRI = 44;
  POLL_HUP = 45;
{ SIGSEGV }
  SEGV_MAPERR = 50;
  SEGV_ACCERR = 51;
  SEGV_KEYERR = 52;
{ SIGTRAP }
  TRAP_BRKPT = 60;
  TRAP_TRACE = 61;
{ GENERAL }
  SI_QUEUE = 71;
  SI_TIMER = 72;
  SI_ASYNCIO = 73;
  SI_MESGQ = 74;


type
  SigSet  =  array[0..wordsinsigset-1] of clong;
  sigset_t= SigSet;
  PSigSet = ^SigSet;
  psigset_t=psigset;
  TSigSet = SigSet;

  psiginfo = ^tsiginfo;
  tsiginfo = record
       si_signo : cint;
       si_errno : cint;
       si_code  : cint;
       si_pid   : pid_t;
       si_uid   : uid_t;
{$ifdef cpu64}
       si_status: cint;
       si_addr  : pointer;
{$else cpu64}
       si_addr  : pointer;
       si_status: cint;
{$endif cpu64}
       si_band  : clong;
       __si_flags: cint;
{$ifdef cpu64}
       __pad: array[0..2] of cint;
{$else cpu64}
       __pad: array[0..5] of cint;
{$endif cpu64}
    end;

{ CPU dependent TSigContext }
{$i sighndh.inc}

type
  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
  SigActionHandler = procedure(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;

  SigActionRec = record  // this needs correct 8-byte alignment for hander
    sa_handler  : SigActionHandler;
    Sa_Mask     : SigSet;
    Sa_Flags    : cint;
  end;
  TSigActionRec = SigActionRec;
  PSigActionRec = ^SigActionRec;

