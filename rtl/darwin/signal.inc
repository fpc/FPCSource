{$PACKRECORDS C}

  {
     $Id$
     Copyright (c) 2000 Apple Computer, Inc. All rights reserved.

     @APPLE_LICENSE_HEADER_START@

     The contents of this file constitute Original Code as defined in and
     are subject to the Apple Public Source License Version 1.1 (the
     "License").  You may not use this file except in compliance with the
     License.  Please obtain a copy of the License at
     http://www.apple.com/publicsource and read it before using this file.

     This Original Code and all software distributed under the License are
     distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
     EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
     INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
     FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
     License for the specific language governing rights and limitations
     under the License.

     @APPLE_LICENSE_HEADER_END@
    }
  { Copyright (c) 1995 NeXT Computer, Inc. All Rights Reserved  }
  {
     Copyright (c) 1982, 1986, 1989, 1991, 1993
        The Regents of the University of California.  All rights reserved.
     (c) UNIX System Laboratories, Inc.
     All or some portions of this file are derived from material licensed
     to the University of California by American Telephone and Telegraph
     Co. or Unix System Laboratories, Inc. and are reproduced herein with
     the permission of UNIX System Laboratories, Inc.

     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions
     are met:
     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
     3. All advertising materials mentioning features or use of this software
        must display the following acknowledgement:
        This product includes software developed by the University of
        California, Berkeley and its contributors.
     4. Neither the name of the University nor the names of its contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.

     THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
     ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
     FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
     OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
     SUCH DAMAGE.

        @(#)signal.h    8.2 (Berkeley) 1/21/94
    }

{$ifdef powerpc}
{$include powerpc/signal.inc}    { sigcontext }
{$else powerpc}
{$ifdef i386}
{$include i386/signal.inc}    { sigcontext }
{$else i386}
{$error Unsupported cpu type!}
{$endif i386}
{$endif powerpc}


  const
  { hangup  }
     SIGHUP = 1;
  { interrupt  }
     SIGINT = 2;
  { quit  }
     SIGQUIT = 3;
  { illegal instruction (not reset when caught)  }
     SIGILL = 4;
  { trace trap (not reset when caught)  }
     SIGTRAP = 5;
  { abort()  }
     SIGABRT = 6;
  { compatibility  }
     SIGIOT = SIGABRT;
  { EMT instruction  }
     SIGEMT = 7;
  { floating point exception  }
     SIGFPE = 8;
  { kill (cannot be caught or ignored)  }
     SIGKILL = 9;
  { bus error  }
     SIGBUS = 10;
  { segmentation violation  }
     SIGSEGV = 11;
  { bad argument to system call  }
     SIGSYS = 12;
  { write on a pipe with no one to read it  }
     SIGPIPE = 13;
  { alarm clock  }
     SIGALRM = 14;
  { software termination signal from kill  }
     SIGTERM = 15;
  { urgent condition on IO channel  }
     SIGURG = 16;
  { sendable stop signal not from tty  }
     SIGSTOP = 17;
  { stop signal from tty  }
     SIGTSTP = 18;
  { continue a stopped process  }
     SIGCONT = 19;
  { to parent on child stop or exit  }
     SIGCHLD = 20;
  { to readers pgrp upon background tty read  }
     SIGTTIN = 21;
  { like TTIN for output if (tp->t_local&LTOSTOP)  }
     SIGTTOU = 22;
  { input/output possible signal  }
     SIGIO = 23;
  { exceeded CPU time limit  }
     SIGXCPU = 24;
  { exceeded file size limit  }
     SIGXFSZ = 25;
  { virtual time alarm  }
     SIGVTALRM = 26;
  { profiling time alarm  }
     SIGPROF = 27;
  { window size changes  }
     SIGWINCH = 28;
  { information request  }
     SIGINFO = 29;
  { user defined signal 1  }
     SIGUSR1 = 30;
  { user defined signal 2  }
     SIGUSR2 = 31;
     SIG_DFL = 0;
     SIG_IGN = 1;
     SIG_ERR = -1;

  type
     sigset_t = dword;

    SignalHandler   = Procedure(Sig : Longint);cdecl;
    PSignalHandler  = ^SignalHandler;
    SignalRestorer  = Procedure;cdecl;
    PSignalRestorer = ^SignalRestorer;
    TSigAction = procedure(Sig: Longint; SigContext: SigContextRec;someptr:pointer);cdecl;

    Sigset=sigset_t;
    PSigSet = ^SigSet;

    SigActionRec = packed record
{
      case byte of
        0: (Sh: SignalHandler; Sa_Flags: longint; Sa_Mask: SigSet);
        1: (sa_handler: TSigAction);
}
      Sa_Handler: SignalHandler;
      Sa_Flags: longint;
      Sa_Mask: SigSet;
   end;

  const
     SA_ONSTACK = $0001;
  { restart system on signal return  }
     SA_RESTART = $0002;
  { disable taking signals on alternate stack  }
     SA_DISABLE = $0004;
  { do not bounce off kernel's sigtramp  }
     SA_USERTRAMP = $0100;
  { do not generate SIGCHLD on child stop  }
     SA_NOCLDSTOP = $0008;
  {
     Flags for sigprocmask:
    }
  { block specified signal set  }
     SIG_BLOCK = 1;
  { unblock specified signal set  }
     SIG_UNBLOCK = 2;
  { set specified signal set  }
     SIG_SETMASK = 3;
  { type of signal function  }
  {
     Structure used in sigaltstack call.
    }
  { signal stack base  }
  { signal stack length  }
  { SA_DISABLE and/or SA_ONSTACK  }

  type
     sigaltstack = record
          ss_sp : ^char;
          ss_size : longint;
          ss_flags : longint;
       end;

  { minimum allowable stack  }

  const
     MINSIGSTKSZ = 8192;
  { recommended stack size  }
     SIGSTKSZ = MINSIGSTKSZ + 32768;
  {
     4.3 compatibility:
     Signal vector "template" used in sigvec call.
    }
  { signal handler  }
  { signal mask to apply  }
  { see signal options below  }

  type
     sigvec = record
          sv_handler : procedure ;
          sv_mask : longint;
          sv_flags : longint;
       end;


  const
     SV_ONSTACK = SA_ONSTACK;
  { same bit, opposite sense  }
     SV_INTERRUPT = SA_RESTART;

(*
  {
     Structure used in sigstack call.
    }
  { signal stack pointer  }
  { current status  }

  type
     sigstack = record
          ss_sp : ^char;
          ss_onstack : longint;
       end;
*)

{
  $Log$
  Revision 1.2  2003-05-25 12:59:57  jonas
    * several fixes, addition of Mach trap numbers (thye are simply syscalls
      with a negative number)

  Revision 1.1  2002/09/08 09:06:07  jonas
    + initial revision, h2pas of sys/signal.h

}
