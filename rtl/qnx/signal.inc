{
    $Id: signal.inc,v 1.1.2.1 2001/12/20 02:55:01 carl Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the types/constants related
    to signal for QNX RTP
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   { requires osposixh types first }
type

   sigset_t = int64;     { used for additional signal   }

   sighandler_t = procedure (signo: cint); cdecl;

   { signal services }
   sigactionrec = packed record
     sa_handler : sighandler_t;   { this is overlapped with sa_sigaction field on solaris }
     sa_flags : cint;
     sa_mask : sigset_t;
   end;


const
    {************************ signals *****************************}
    { more can be provided. Herein are only included the required  }
    { values.                                                      }
    {**************************************************************}
    SIGABRT    = 6;     { abnormal termination           }
    SIGALRM    =14;     { alarm clock (used with alarm() }
    SIGFPE     = 8;     { illegal arithmetic operation   }
    SIGHUP     = 1;     { Hangup                         }
    SIGILL     = 4;     { Illegal instruction            }
    SIGINT     = 2;     { Interactive attention signal   }
    SIGKILL    = 9;     { Kill, cannot be caught         }
    SIGPIPE    =13;     { Broken pipe signal             }
    SIGQUIT    = 3;     { Interactive termination signal }
    SIGSEGV    =11;     { Detection of invalid memory reference }
    SIGTERM    =15;     { Termination request           }
    SIGUSR1    =16;     { Application defined signal 1  }
    SIGUSR2    =17;     { Application defined signal 2  }
    SIGCHLD    =18;     { Child process terminated / stopped }
    SIGCONT    =25;     { Continue if stopped               }
    SIGSTOP    =23;     { Stop signal. cannot be cuaght     }
    SIGSTP     =24;     { Interactive stop signal           }
    SIGTTIN    =26;     { Background read from TTY          }
    SIGTTOU    =27;     { Background write to TTY           }
    SIGBUS     =10;     { Access to undefined memory        }
    { --------------- QNX specific signals ---------------  }
    SIGTRAP    = 5;     { trace trap (not reset when caught) }
    SIGIOT     = 6;     { IOT instruction                    } 
    SIGEMT     = 7;     { EMT instruction                    }
    SIGDEADLK  = 7; { Mutex deadlock                     }
    SIGSYS     =12;     { bad argument to system call        }
    SIGCLD     =SIGCHLD;
    SIGPWR     =19;     { power-fail restart                 }
    SIGWINCH   =20;     { window change                      }
    SIGURG     =21;     { urgent condition on I/O channel    }
    SIGPOLL    =22;     { System V name for SIGIO            }
    SIGIO      =SIGPOLL;
    SIGVTALRM  =28;     { virtual timer expired              }
    SIGPROF    =29;     { profileing timer expired           }
    SIGXCPU    =30;     { exceded cpu limit                  }
    SIGXFSZ    =31;     { exceded file size limit            }
    

{
  $Log: signal.inc,v $
  Revision 1.1.2.1  2001/12/20 02:55:01  carl
  + QNX versions (still untested)

}