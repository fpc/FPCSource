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

{$packrecords C}

{********************
      Signal
********************}

Const
  { For sending a signal }
{$ifdef SPARC}
  SA_SIGINFO   = $200;
  SA_NOMASK    = $20;

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 4;
{$else SPARC}
  SA_NOCLDSTOP = 1;
  SA_NOCLDWAIT = 2;
  SA_SIGINFO   = 4;
  SA_RESTORER  = $04000000;
  SA_ONSTACK   = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NODEFER   = $40000000;
  SA_RESETHAND = $80000000;

  SA_NOMASK    = SA_NODEFER;
  SA_ONESHOT   = SA_RESETHAND;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;
{$endif SPARC}

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

{ si_code field values for tsiginfo.si_code when si_signo = SIGFPE }
const
  FPE_INTDIV = 1; { integer divide by zero }
  FPE_INTOVF = 2; { integer overflow }
  FPE_FLTDIV = 3; { floating point divide by zero }
  FPE_FLTOVF = 4; { floating point overflow }
  FPE_FLTUND = 5; { floating point underflow }
  FPE_FLTRES = 6; { floating point inexact result }
  FPE_FLTINV = 7; { floating point invalid operation }
  FPE_FLTSUB = 8; { floating point subscript out of range }

const
  SI_PAD_SIZE   = ((128 div sizeof(longint)) - 3);

type
  sigset_t = array[0..wordsinsigset-1] of cuLong;
  tsigset  = sigset_t;
  sigset   = sigset_t;
  psigset  = ^tsigset;

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

{ CPU dependent TSigContext }
{$i sighndh.inc}

type
  signalhandler_t = procedure(signal: longint); cdecl;
  sigactionhandler_t = procedure(signal: longint; info: psiginfo; context: psigcontext); cdecl;
  sigrestorerhandler_t = procedure; cdecl;
      
  signalhandler = signalhandler_t;
  sigactionhandler = sigactionhandler_t;
  sigrestorerhandler = sigrestorerhandler_t;
  tsignalhandler = signalhandler_t;
  tsigactionhandler = sigactionhandler_t;
  tsigrestorerhandler = sigrestorerhandler_t;
  
  psigactionrec = ^sigactionrec;
  sigactionrec = record
    sa_handler: sigactionhandler_t;
    sa_flags: culong;
    sa_restorer: sigrestorerhandler_t;
    sa_mask: sigset_t;
  end;

