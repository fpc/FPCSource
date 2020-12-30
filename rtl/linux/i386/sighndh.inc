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

  tfpxreg = record
    significand: array[0..3] of word;
    exponent: word;
    padding : array[0..2] of word;
  end;

  txmmreg = record
    element : array[0..3] of dword;
  end;

  tfpx_sw_bytes = record
    magic1 : dword;
    extended_size : dword;
    xfeatures : qword;
    xstate_size : dword;
    padding : array[0..6] of dword;
  end;


  pfpstate = ^tfpstate;
  tfpstate = record
    cw, sw, tag, ipoff, cssel, dataoff, datasel: cardinal;
    st: array[0..7] of tfpreg;
    status, magic: word;
    fxsr_env : array[0..5] of dword;
    mxcsr : dword;
    reserved : dword;
    fxsr_st : array[0..7] of tfpxreg;
    xmmreg : array[0..7] of txmmreg;
    case byte of
       1: (padding : array[0..43] of dword;
           case byte of
             1: (padding2 : array[0..11] of dword);
             2: (sw_reserved : tfpx_sw_bytes);
          );
       2: (padding1 : array[0..43] of dword);
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

  tsigaltstack=record
        ss_sp : pointer;
        ss_flags : longint;
        ss_size : longint;
  end;

  Pucontext=^Tucontext;
  TUcontext=record
    uc_flags : cardinal;
    uc_link  : Pucontext;
    uc_stack : tsigaltstack;
    uc_mcontext : tsigcontext;
    uc_sigmask : tsigset;
  end;

