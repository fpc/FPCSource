{
    $Id$
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

   { requires osposixh types first }
type
   sigset_t = array[0..3] of cardinal; { used for additional signal   }

   sighandler_t = procedure (signo: cint); cdecl;

   { signal services }
   sigactionrec = packed record
     sa_flags : cint;
     sa_handler : sighandler_t;   { this is overlapped with sa_sigaction field on solaris }
     sa_mask : sigset_t;
     sa_resv : array[1..2] of cint;  { for non-_LP64 platforms only                       }
   end;

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

    SIGTRAP    =  5;	{ trace trap (not reset when caught) }
    SIGIOT     =  6;	{ IOT instruction }
    SIGEMT     =  7;	{ EMT instruction }
    SIGSYS     = 12;	{ bad argument to system call }
    SIGCLD     = 18;	{ child status change }
    SIGPWR     = 19;	{ power-fail restart }
    SIGWINCH   = 20;	{ window size change }
    SIGURG     = 21;	{ urgent socket condition }
    SIGPOLL    = 22;	{ pollable event occured }
    SIGIO      = SIGPOLL;{ socket I/O possible (SIGPOLL alias) }
    SIGVTALRM  = 28;	{ virtual timer expired }
    SIGPROF    = 29;	{ profiling timer expired }
    SIGXCPU    = 30;	{ exceeded cpu limit }
    SIGXFSZ    = 31;	{ exceeded file size limit }
    SIGWAITING = 32;	{ process's lwps are blocked }
    SIGLWP     = 33;	{ special signal used by thread library }
    SIGFREEZE  = 34;	{ special signal used by CPR }
    SIGTHAW    = 35;	{ special signal used by CPR }
    SIGCANCEL  = 36;	{ thread cancellation signal used by libthread }
    SIGLOST    = 37;	{ resource lost (eg, record-lock lost) }

{
  $Log$
  Revision 1.2  2004-11-06 22:22:28  florian
    * some sunos stuff from 1.0.x merged
}
