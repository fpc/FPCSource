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
  SA_SHIRQ     = $04000000;
  SA_STACK     = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NOMASK    = $40000000;
  SA_ONESHOT   = $80000000;

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


const
  SI_PAD_SIZE   = ((128 div sizeof(longint)) - 3);

  
type
  SigSet  =  array[0..wordsinsigset-1] of cint;
  sigset_t= SigSet;
  PSigSet = ^SigSet;
  psigset_t=psigset;
  TSigSet = SigSet;
  
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
  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;
{$ifdef CPUARM}  
  SigActionHandler = procedure(Sig: longint; _a2,_a3,_a4 : dword; SigContext: PSigInfo; uContext : PuContext); cdecl;
{$else}  
  SigActionHandler = procedure(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
{$endif}

{$ifdef CPUARM}
{$define NEWSIGNAL}
{$endif CPUARM}

{$ifdef CPUx86_64}
{$define NEWSIGNAL}
{$endif CPUx86_64}

  SigActionRec = packed record  // this is temporary for the migration
    sa_handler : SigActionHandler;
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
  Revision 1.24  2005-02-05 22:45:54  peter
    * sigactionhandler fixed for arm

  Revision 1.23  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

  Revision 1.22  2004/08/04 19:27:10  florian
    * fixed floating point and integer exception handling on sparc/linux

  Revision 1.21  2004/05/31 09:08:14  peter
    * added siginfo const

  Revision 1.20  2004/05/27 23:15:43  peter
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

