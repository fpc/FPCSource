{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the types/constants related
    to signal for Solaris.

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
  SIGABRT    =  6;     { abnormal termination           }
  SIGALRM    = 14;     { alarm clock (used with alarm() }
  SIGFPE     =  8;     { illegal arithmetic operation   }
  SIGHUP     =  1;     { Hangup                         }
  SIGILL     =  4;     { Illegal instruction            }
  SIGINT     =  2;     { Interactive attention signal   }
  SIGKILL    =  9;     { Kill, cannot be caught         }
  SIGPIPE    = 13;     { Broken pipe signal             }
  SIGQUIT    =  3;     { Interactive termination signal }
  SIGSEGV    = 11;     { Detection of invalid memory reference }
  SIGTERM    = 15;     { Termination request           }
  SIGUSR1    = 16;     { Application defined signal 1  }
  SIGUSR2    = 17;     { Application defined signal 2  }
  SIGCHLD    = 18;     { Child process terminated / stopped }
  SIGCONT    = 25;     { Continue if stopped               }
  SIGSTOP    = 23;     { Stop signal. cannot be cuaght     }
  SIGSTP     = 24;     { Interactive stop signal           }
  SIGTTIN    = 26;     { Background read from TTY          }
  SIGTTOU    = 27;     { Background write to TTY           }
  SIGBUS     = 10;     { Access to undefined memory        }

  { Solaris specific signals                               }

  SIGTRAP    =  5;      { trace trap (not reset when caught) }
  SIGIOT     =  6;      { IOT instruction }
  SIGEMT     =  7;      { EMT instruction }
  SIGSYS     = 12;      { bad argument to system call }
  SIGCLD     = 18;      { child status change }
  SIGPWR     = 19;      { power-fail restart }
  SIGWINCH   = 20;      { window size change }
  SIGURG     = 21;      { urgent socket condition }
  SIGPOLL    = 22;      { pollable event occured }
  SIGIO      = SIGPOLL;{ socket I/O possible (SIGPOLL alias) }
  SIGVTALRM  = 28;      { virtual timer expired }
  SIGPROF    = 29;      { profiling timer expired }
  SIGXCPU    = 30;      { exceeded cpu limit }
  SIGXFSZ    = 31;      { exceeded file size limit }
  SIGWAITING = 32;      { process's lwps are blocked }
  SIGLWP     = 33;      { special signal used by thread library }
  SIGFREEZE  = 34;      { special signal used by CPR }
  SIGTHAW    = 35;      { special signal used by CPR }
  SIGCANCEL  = 36;      { thread cancellation signal used by libthread }
  SIGLOST    = 37;      { resource lost (eg, record-lock lost) }

  SIG_BLOCK   = 1;
  SIG_UNBLOCK = 2;
  SIG_SETMASK = 3;

  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

  { definitions for the sa_flags field }
  SA_ONSTACK   = $00000001;
  SA_RESETHAND = $00000002;
  SA_RESTART   = $00000004;
  SA_SIGINFO   = $00000008;
  SA_NODEFER   = $00000010;
  SA_NOCLDWAIT = $00010000;
  SA_WAITSIG   = $00010000;

{$ifdef cpu64}
  SI_PAD_SIZE   = ((256 div sizeof(cint)) - 4);
{$else}
  SI_PAD_SIZE   = ((128 div sizeof(cint)) - 3);
{$endif}

type
  SigSet  =  array[0..wordsinsigset-1] of cint;
  sigset_t= SigSet;
  PSigSet = ^SigSet;
  psigset_t=psigset;
  TSigSet = SigSet;

  t_pdata = record
    case longint of
      1 : ( _kill : record
              _uid   : uid_t;
              _value : cint; { signal sent? }
            end );
      2 : ( _cld : record
              _utime  : clock_t;
              _status : cint;
              _stime  : clock_t;
            end );
  end;

  t_proc = record
    _pid: pid_t;
    _pdata: t_pdata;
  end;

  psiginfo = ^tsiginfo;
  tsiginfo = record
       si_signo : cint;
       si_code  : cint;
       si_errno : cint;
{$ifdef cpu64}
       si_pad   : cint;
{$endif cpu64}
       _sifields : record
           case longint of
              0 : ( _pad : array[0..(SI_PAD_SIZE)-1] of cint );
              1 : ( _proc : t_proc);
              3 : ( _rctl : record
                   _entity : longint; { int32_t }
                end );
              5 : ( _sigfault : record
                   _addr : pointer;
                   _trapno : cint;
                   _pc    : pointer;
                end );
              6 : ( _sigpoll : record
                   _fd : longint;
                   _band : longint;
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
  SigActionHandler = procedure(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;

  SigActionRec = packed record  // this is temporary for the migration
    Sa_Flags    : cuint;
    sa_handler  : SigActionHandler;
    Sa_Mask     : SigSet;
    sa_resv : array[1..2] of cint;  { for non-_LP64 platforms only                       }
  end;
  TSigActionRec = SigActionRec;
  PSigActionRec = ^SigActionRec;

