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
  {
     Copyright (c) 1992, 1993 NeXT Computer, Inc.

     HISTORY

        Machine specific signal information.

     HISTORY
     25-MAR-97  Umesh Vaishampayan (umeshv@NeXT.com)
        Ported from m98k and hppa.

     13-Jan-92  Peter King (king) at NeXT Computer, Inc.
        Filled out struct sigcontext to hold all registers.
        Added regs_saved_t to specify which regs stored in the
        sigcontext are valid.

     09-Nov-92  Ben Fathi (benf) at NeXT, Inc.
        Ported to m98k.

     09-May-91  Mike DeMoney (mike) at NeXT, Inc.
        Ported to m88k.
    }
  const
     _PPC_SIGNAL_ = 1;

  type

     sig_atomic_t = longint;
  {
     Machine-dependant flags used in sigvec call.
    }
  { Save all regs in sigcontext  }

  const
     SV_SAVE_REGS = $1000;
  {
     regs_saved_t -- Describes which registers beyond what the kernel cares
                   about are saved to and restored from this sigcontext.

     The default is REGS_SAVED_CALLER, only the caller saved registers
     are saved.  If the SV_SAVE_REGS flag was set when the signal
     handler was registered with sigvec() then all the registers will be
     saved in the sigcontext, and REGS_SAVED_ALL will be set.  The C
     library uses REGS_SAVED_NONE in order to quickly restore kernel
     state during a longjmp().
    }
  { Only kernel managed regs restored  }
  { "Caller saved" regs: rpc, a0-a7,
                                           t0-t4, at, lk0-lk1, xt1-xt20,
                                           xr0-xr1  }
  { All registers  }

  type

     regs_saved_t = (REGS_SAVED_NONE,REGS_SAVED_CALLER,REGS_SAVED_ALL
       );
  {
     Information pushed on stack when a signal is delivered.
     This is used by the kernel to restore state following
     execution of the signal handler.  It is also made available
     to the handler to allow it to properly restore state if
     a non-standard exit is performed.
    }
  { sigstack state to restore  }
  { signal mask to restore  }
  { pc  }
  { processor status word  }
  { stack pointer if sc_regs == NULL  }
  { (kernel private) saved state  }
     sigcontextrec = record
          sc_onstack : longint;
          sc_mask : longint;
          sc_ir : longint;
          sc_psw : longint;
          sc_sp : longint;
          sc_regs : pointer;
       end;

{
  $Log$
  Revision 1.1  2002-09-08 09:01:48  jonas
    + initial revision

}