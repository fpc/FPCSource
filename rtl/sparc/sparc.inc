{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2004 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    Sparc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                           SPARC specific stuff
****************************************************************************}
function get_fsr : dword;assembler;nostackframe;
  var
    fsr : dword;
  asm
    st %fsr,fsr
    ld fsr,%o0
  end;


function get_got : pointer;assembler;nostackframe;[public, alias: 'FPC_GETGOT'];
  asm
    retl
    add %o7,%l7,%l7
  end;


procedure set_fsr(fsr : dword);assembler;
  var
    _fsr : dword;
  asm
    // force memory location
    st fsr,_fsr
    ld _fsr,%fsr
  end;


procedure fpc_cpuinit;
  begin
    { enable div by 0 and invalid operation fpu exceptions }
    { round towards zero; ieee compliant arithmetics }
    set_fsr((get_fsr and $3fbfffff) or $09000000);
  end;


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    mov %fp,%o0
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;nostackframe;
  asm
    { framebp = %o0 }
    { flush register windows, so they are stored in the stack }
    ta      3
    ld [%o0+60],%o0
    { add 8 to skip jmpl and delay slot }
    add %o0,8,%o0
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;nostackframe;
  asm
    { flush register windows, so they are stored in the stack }
    ta      3
    { framebp = %o0 }
    ld [%o0+56],%o0
  end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr:Pointer;assembler;nostackframe;
  asm
    mov %sp,%o0
  end;


{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:longint);[public, alias: 'FPC_MOVE'];assembler;
{
  Registers:
    %l0 temp. to do copying
    %l1 inc/decrement
    %l2/l3/l4/l5 qword move
}
  asm
    // count < 0 ?
    cmp %g0,%i2
    bge .Lmoveexit
    nop

    // possible overlap?
    cmp %i0,%i1
    bcc .Lnopossibleoverlap
    nop
    // source < dest ....
    add %i0,%i2,%l0
    // overlap?
    cmp %l0,%i1
    // source+count < dest ?
    bcs .Lnopossibleoverlap
    nop

  .Lcopybackward:
    // check alignment of source and dest
    or %i0,%i1,%l0

    // move src and dest to the end of the blocks
    // assuming 16 byte block size
    sub %i2,1,%l1
    add %i0,%l1,%i0
    add %i1,%l1,%i1
{
    // everything 16 byte aligned ?
    andcc %l0,15,%l0
    be .Lmovetwordwise
    // load direction in delay slot
    mov -16,%l1

    // adjust according to block size
    add %i0,8,%i0
    add %i1,8,%i1
    andcc %l0,7,%l0
    be .Lmoveqwordwise
    mov -8,%l1

// adjust according to block size
    add %i0,4,%i0
    add %i1,4,%i1
    andcc %l0,3,%l0
    be .Lmovedwordwise
    mov -4,%l1

// adjust according to block size
    add %i0,2,%i0
    add %i1,2,%i1
    andcc %l0,1,%l0
    be .Lmovewordwise
    mov -2,%l1

// adjust according to block size
    add %i0,1,%i0
    add %i1,1,%i1
}
    ba .Lmovebytewise
    mov -1,%l1

  .Lnopossibleoverlap:
    // check alignment of source and dest
    or %i0,%i1,%l0

    // everything 16 byte aligned ?
    andcc %l0,15,%l0
    be .Lmovetwordwise
    // load direction in delay slot
    mov 16,%l1
    andcc %l0,7,%l0
    be .Lmoveqwordwise
    mov 8,%l1
    andcc %l0,3,%l0
    be .Lmovedwordwise
    mov 4,%l1
    andcc %l0,1,%l0
    be .Lmovewordwise
    mov 2,%l1
    ba .Lmovebytewise
    mov 1,%l1

  .Lmovetwordwise:
    srl %i2,4,%l6
    cmp %g0,%l6
    sll %l6,4,%l7
    be .Lmoveqwordwise_shift
    nop

  .Lmovetwordwise_loop:
    ld [%i0],%l2
    ld [%i0+4],%l3
    subcc %l6,1,%l6
    ld [%i0+8],%l4
    ld [%i0+12],%l5
    add %i0,%l1,%i0
    st  %l2,[%i1]
    st  %l3,[%i1+4]
    st  %l4,[%i1+8]
    st  %l5,[%i1+12]
    add %i1,%l1,%i1
    bne .Lmovetwordwise_loop
    nop
    subcc %i2,%l7,%i2
    be .Lmoveexit
    nop

  .Lmoveqwordwise_shift:
    sra %l1,1,%l1
  .Lmoveqwordwise:
    srl %i2,3,%l6
    cmp %g0,%l6
    sll %l6,3,%l7
    be .Lmovedwordwise_shift
    nop

  .Lmoveqwordwise_loop:
    ld [%i0],%l2
    ld [%i0+4],%l3
    subcc %l6,1,%l6
    add %i0,%l1,%i0
    st  %l2,[%i1]
    st  %l3,[%i1+4]
    add %i1,%l1,%i1
    bne .Lmoveqwordwise_loop
    nop
    subcc %i2,%l7,%i2
    be .Lmoveexit
    nop

  .Lmovedwordwise_shift:
    sra %l1,1,%l1
  .Lmovedwordwise:
    srl %i2,2,%l6
    cmp %g0,%l6
    sll %l6,2,%l7
    be .Lmovewordwise_shift
    nop

  .Lmovedwordwise_loop:
    ld [%i0],%l0
    subcc %l6,1,%l6
    add %i0,%l1,%i0
    st %l0,[%i1]
    add %i1,%l1,%i1
    bne .Lmovedwordwise_loop
    nop
    subcc %i2,%l7,%i2
    be .Lmoveexit
    nop

  .Lmovewordwise_shift:
    sra %l1,1,%l1
  .Lmovewordwise:
    srl %i2,1,%l6
    cmp %g0,%l6
    sll %l6,1,%l7
    be .Lmovebytewise_shift
    nop

  .Lmovewordwise_loop:
    lduh [%i0],%l0
    subcc %l6,1,%l6
    add %i0,%l1,%i0
    sth %l0,[%i1]
    add %i1,%l1,%i1
    bne .Lmovewordwise_loop
    nop
    subcc %i2,%l7,%i2
    be .Lmoveexit
    nop

  .Lmovebytewise_shift:
    sra %l1,1,%l1
  .Lmovebytewise:
    cmp %g0,%i2
    be .Lmoveexit
    nop

    ldub [%i0],%l0
    subcc %i2,1,%i2
    add %i0,%l1,%i0
    stb %l0,[%i1]
    add %i1,%l1,%i1
    bne .Lmovebytewise
    nop
  .Lmoveexit:
  end;


{****************************************************************************
                               Integer math
****************************************************************************}

{$define FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint; assembler;{$ifdef SYSTEMINLINE}inline;{$endif}nostackframe;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_abs];{$endif}
asm
  sra %o0,31,%g1
  add %o0,%g1,%o0
  xor %o0,%g1,%o0
end;

var
  fpc_system_lock : byte;export name 'fpc_system_lock';


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l : longint) : boolean;assembler;nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.Ldeclocked1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .Ldeclocked1
  nop

  ld [%o0],%g1
  sub %g1,1,%g1
  st %g1,[%o0]

  subcc %g1,1,%g0
  addx %g0,%g0,%o0

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler;nostackframe;
asm
  { usually, we shouldn't lock here so saving the stack frame for these extra intructions is
    worse the effort, especially while waiting :)
  }
.Linclocked1:
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  ldstub [%g1],%g1
  cmp %g1,0
  bne .Linclocked1
  nop

  ld [%o0],%g1
  add %g1,1,%g1
  st %g1,[%o0]

  { unlock }
  sethi %hi(fpc_system_lock), %g1
  or %g1,%lo(fpc_system_lock), %g1
  stb %g0,[%g1]
end;


{
  $Log$
  Revision 1.18  2005-01-27 21:26:39  florian
    + getgot function added

  Revision 1.17  2005/01/18 20:37:26  florian
    * set floating point precision and ieee compliance

  Revision 1.16  2004/11/21 19:11:33  peter
    * fix bootstrapping

  Revision 1.15  2004/11/21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.14  2004/11/03 20:53:58  florian
    * get_frame fixed

  Revision 1.13  2004/10/14 19:45:39  florian
    + added and implemented functions for locked operations

  Revision 1.12  2004/10/03 12:41:30  florian
    * made sqrt, sqr and abs internal for the sparc

  Revision 1.11  2004/10/02 20:46:20  florian
    * made assembler implementation of move

  Revision 1.10  2004/09/23 11:30:41  florian
    * fixed indention

  Revision 1.9  2004/09/12 12:04:23  peter
    * restore traps when returning with longjmp

  Revision 1.8  2004/08/04 19:27:10  florian
    * fixed floating point and integer exception handling on sparc/linux

  Revision 1.7  2004/05/30 20:03:05  florian
    * ?

  Revision 1.6  2004/05/27 23:34:37  peter
    * backtrace support

  Revision 1.5  2004/01/02 17:22:14  jonas
    + fpc_cpuinit procedure to allow cpu/fpu initialisation before any unit
      initialises
    + fpu exceptions for invalid operations and division by zero enabled for
      ppc

  Revision 1.4  2003/12/04 21:42:07  peter
    * register calling updates

  Revision 1.3  2003/03/17 14:30:11  peter
    * changed address parameter/return values to pointer instead
      of longint

  Revision 1.2  2003/02/05 21:48:34  mazen
  * fixing run time errors related to unimplemented abstract methods in CG
  + giving empty emplementations for some RTL functions

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}
