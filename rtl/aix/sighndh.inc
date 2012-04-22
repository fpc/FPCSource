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

type
  plabel_t = ^label_t;
  label_t = record
    prev: plabel_t;
    iar: culong;
    stack: culong;
    toc: culong;
    cr: culong;
    intpri: culong;
    reg: array[0..18] of culong; // non-volatile regs (13..31)
  end;

  padspace_t = ^adspace_t;
  adspace_t = record
    alloc: culong;
    srval: array[0..15] of cuint;
  end;

  pmstsave = ^mstsave;
  mstsave = record
    prev: pmstsave;
    kjmpbuf: plabel_t;
    stackfix: pchar;
    intpri: byte;
    backt: byte;
    rsvd: array[0..1] of byte;
    curid: pid_t;

    excp_type: cint;
    iar: culong; // instruction address register
    msr: culong;
    cr: culong;
    lr: culong;
    ctr: culong;
    xer: culong;
    mq: culong;
    tid: culong;
    fpscr: culong;
    fpeu: bytebool;
    fpinfo: byte;
    pad: array[0..1] of byte;
    except_: array[0..4] of culong;
    pad1: array[0..3] of byte;
    o_iar: culong;
    o_toc: culong;
    o_arg1: culong;
    excbranch: culong;
    fpscrx: culong;
    o_vaddr: culong;
    cachealign: array[0..6] of culong;
    as_: adspace_t;
    gpr: array[0..31] of culong;
    fpr: array[0..31] of double;
  end;

  pstack_t = ^stack_t;
  stack_t = record
    ss_sp: pointer;
    ss_size: size_t;
    ss_flags: cint;
    __pad: array[0..3] of cint;
  end;

  __vmxreg_t = record
    __v: array[0..3] of cuint;
  end;

  __vmx_context = record
    __vr: array[0..31] of __vmxreg_t;
    __pad1: array[0..2] of cuint;
    __vscr: cuint;
    __vrsave: cuint;
    __pad2: array[0..1] of cuint;
  end;

  p__extctx_t = ^__extctx_t;
  __extctx_t = record
    __flags: cuint;
    __rsvd1: array[0..2] of cuint;
    __u1: record
      __vmx: __vmx_context
    end;
    __ukeys: array[0..1] of cuint;
    __reserved: array[0..4096-sizeof(__vmx_context)-7*sizeof(cint)-1] of byte;
    __extctx_magic: cint;
  end;

{$ifdef cpu64}
  __context64 = record
    gpr: array[0..31] of culonglong;
    msr: culonglong;
    iar: culonglong;
    lr: culonglong;
    ctr: culonglong;
    cr: cuint;
    xer: cuint;
    fpscr: cuint;
    fpscrx: cuint;
    except_: array[0..0] of culonglong;
    fpr: array[0..31] of double;
    fpeu: byte;
    fpinfo: byte;
    pad: array[0..1] of byte;
    excp_type: cint;
  end;

  mcontext_t = __context64;
{$else}
  mcontext_t = mstsave;
{$endif}

  pucontext_t = ^tucontext_t;
  PSigContext = pucontext_t;
  tucontext_t = record
    __sc_onstack: cint;
    uc_sigmask: sigset_t;
    __sc_uerror: cint;
    uc_mcontext: mcontext_t;
    uc_link: pucontext_t;
    uc_stack: stack_t;
    __extctx: p__extctx_t;
    __extctx_magic: cint;
    __pad: array[0..{$ifdef cpu64}0{$else}1{$endif}] of cint;
  end;

