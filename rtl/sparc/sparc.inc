{

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
function get_fsr : dword;assembler;nostackframe;[public, alias: 'FPC_GETFSR'];
  var
    fsr : dword;
  asm
    st %fsr,fsr
    ld fsr,%o0
  end;


procedure set_fsr(fsr : dword);assembler;[public, alias: 'FPC_SETFSR'];
  var
    _fsr : dword;
  asm
    // force memory location
    st fsr,_fsr
    ld _fsr,%fsr
  end;


function get_got : pointer;assembler;nostackframe;[public, alias: 'FPC_GETGOT'];
  asm
    retl
    add %o7,%l7,%l7
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


{$ifndef FPC_SYSTEM_HAS_MOVE}
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
{$endif FPC_SYSTEM_HAS_MOVE}


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
  $Log: sparc.inc,v $
  Revision 1.21  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.20  2005/02/13 18:58:27  florian
    + FPU controll routines in math unit

  Revision 1.19  2005/02/07 22:17:48  peter
    * add $ifdef for move

  Revision 1.18  2005/01/27 21:26:39  florian
    + getgot function added

  Revision 1.17  2005/01/18 20:37:26  florian
    * set floating point precision and ieee compliance

}
