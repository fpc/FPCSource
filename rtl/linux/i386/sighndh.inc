{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    Sigcontext and Sigaction

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

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

  PSigContext = ^TSigContext;
  TSigContext = record
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

{
  $Log: sighndh.inc,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.1  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}

