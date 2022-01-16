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
function get_fsr : dword;assembler;[public, alias: 'FPC_GETFSR'];
  var
    fsr : dword;
  asm
    st %fsr,fsr
    ld fsr,%l0
    st %l0,__result
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


{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    { enable div by 0 and invalid operation fpu exceptions
      round towards zero; ieee compliant arithmetics }
    set_fsr((get_fsr and $3fbfffff) or $09000000);
  end;

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


procedure fpc_cpuinit;
  begin
    SysResetFPU;
    if not(IsLibrary) then
      SysInitFPU;
  end;


{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    mov %fp,%o0
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;nostackframe;
  asm
    { framebp = %o0 }
    subcc   %o0,0,%o0
    be      .Lframezero
    nop
    { flush register windows, so they are stored in the stack }
    flushw
    ldx [%o0+STACK_BIAS+120],%o0
    { check if new %o0 register is zero }
    subcc   %o0,0,%o0
    be      .Lframezero
    nop
    { if not zero, add 8 to skip jmpl and delay slot }
    add %o0,8,%o0
.Lframezero:
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;nostackframe;
  asm
    { framebp = %o0 }
    subcc   %o0,0,%o0
    be      .Lframezero
    nop
    { flush register windows, so they are stored in the stack }
    flushw
    ldx [%o0+STACK_BIAS+112],%o0
.Lframezero:
  end;


{$define FPC_SYSTEM_HAS_SPTR}
function Sptr:Pointer;assembler;nostackframe;
  asm
    mov %sp,%o0
  end;


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:{$ifdef MOVE_HAS_SIZEUINT_COUNT}SizeUInt{$else}SizeInt{$endif});[public, alias: 'FPC_MOVE'];assembler;
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

var
  fpc_system_lock : byte;export name 'fpc_system_lock';


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l : longint) : boolean;assembler;nostackframe;
asm
.Ldeclocked1:
  ld [%o0],%g4
  sub %g4,1,%g1
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .Ldeclocked1
  sub %g1,1,%g1
  movrz %g1,1,%o0
  movrnz %g1,0,%o0
end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler;nostackframe;
asm
.Linclocked1:
  ld [%o0],%g4
  add %g4,1,%g1
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .Linclocked1
  nop
end;


function InterLockedDecrement (var Target: longint) : longint;assembler;nostackframe;
asm
.LInterLockedDecrement1:
  ld [%o0],%g4
  sub %g4,1,%g1
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .LInterLockedDecrement1
  nop
  sub %g1,1,%o0
end;


function InterLockedIncrement (var Target: longint) : longint;assembler;nostackframe;
asm
.LInterLockedIncrement1:
  ld [%o0],%g4
  add %g4,1,%g1
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .LInterLockedIncrement1
  nop
  add %g1,1,%o0
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;assembler;nostackframe;
asm
.LInterLockedExchange1:
  mov %o1,%g1
  ld [%o0],%g4
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .LInterLockedExchange1
  nop
  mov %g1,%o0
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;assembler;nostackframe;
asm
.LInterLockedExchangeAdd1:
  ld [%o0],%g4
  add %g4,%o1,%g1
  cas [%o0],%g4,%g1
  cmp %g4,%g1
  bne .LInterLockedExchangeAdd1
  nop
  mov %g1,%o0
end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;assembler;nostackframe;
asm
  cas [%o0],%o2,%o1
  mov %o1,%o0
end;


function InterLockedDecrement64(var Target: Int64) : Int64;assembler;nostackframe;
asm
.LInterLockedDecrement641:
  ldx [%o0],%g4
  sub %g4,1,%g1
  casx [%o0],%g4,%g1
  cmp %g4,%g1
  bne %xcc,.LInterLockedDecrement641
  nop
  sub %g1,1,%o0
end;


function InterLockedIncrement64(var Target: Int64) : Int64;assembler;nostackframe;
asm
.LInterLockedIncrement641:
  ldx [%o0],%g4
  add %g4,1,%g1
  casx [%o0],%g4,%g1
  cmp %g4,%g1
  bne %xcc,.LInterLockedIncrement641
  nop
  add %g1,1,%o0
end;


function InterLockedExchange64(var Target: Int64;Source : Int64) : Int64;assembler;nostackframe;
asm
.LInterLockedExchange641:
  mov %o1,%g1
  ldx [%o0],%g4
  casx [%o0],%g4,%g1
  cmp %g4,%g1
  bne %xcc,.LInterLockedExchange641
  nop
  mov %g1,%o0
end;


function InterLockedExchangeAdd64(var Target: Int64;Source : Int64) : Int64;assembler;nostackframe;
asm
.LInterLockedExchangeAdd641:
  ldx [%o0],%g4
  add %g4,%o1,%g1
  casx [%o0],%g4,%g1
  cmp %g4,%g1
  bne %xcc,.LInterLockedExchangeAdd641
  nop
  mov %g1,%o0
end;


function InterlockedCompareExchange64(var Target: Int64; NewValue: Int64; Comperand: Int64): Int64;assembler;nostackframe;
asm
  casx [%o0],%o2,%o1
  mov %o1,%o0
end;

{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}
{$define FPC_SYSTEM_HAS_MEM_BARRIER}

const
  LoadLoad   = $01;
  StoreLoad  = $02;
  LoadStore  = $04;
  StoreStore = $08;
  LookAside  = $10;
  MemIssue   = $20;
  Sync       = $40;

{$if not(defined(SPARCV7)) and not(defined(SPARCV8))}
{$define CPUSPARC_HAS_MEMBAR}
{$endif}

procedure ReadBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifdef CPUSPARC_HAS_MEMBAR}
  ba,pt .L1
  membar LoadLoad
.L1:
{$endif}
end;

procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  { reads imply barrier on earlier reads depended on }
end;

procedure ReadWriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifdef CPUSPARC_HAS_MEMBAR}
  ba,pt .L1
  membar LoadLoad + LoadStore + StoreLoad + StoreStore
.L1:
{$endif}
end;

procedure WriteBarrier;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
{$ifdef CPUSPARC_HAS_MEMBAR}
  ba,pt .L1
  stbar
.L1:
{$endif}
end;

{$endif}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
{ %o0=high(AValue) %o1=low(AValue), result: %o0:%o1 }
    and    %o2,63,%o2
    subcc  %o2,32,%g0
    bcc    .L1
    nop
    srl    %o1,%o2,%o1
    subcc  %o2,%g0,%g0
    be     .Lexit
    sra    %o0,%o2,%o0
    sub    %g0,%o2,%o3
    sll    %o0,%o3,%o3
    ba     .Lexit
    or     %o3,%o1,%o1
.L1:
    sra    %o0,%o2,%o1
    sra    %o0,31,%o0
.Lexit:
end;
{$endif FPC_SYSTEM_HAS_SAR_QWORD}

