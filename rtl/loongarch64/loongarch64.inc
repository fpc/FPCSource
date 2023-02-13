{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    RiscV64

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  fpu_i = 1 shl 0;
  fpu_u = 1 shl 1;
  fpu_o = 1 shl 2;
  fpu_z = 1 shl 3;
  fpu_v = 1 shl 4;


function getrm: dword; nostackframe; assembler;
  asm
    movfcsr2gr $a0, $r3
    srli.w $a0, $a0, 8
  end;


procedure __setrm(val: dword); nostackframe; assembler;
  asm
    slli.w $a0, $a0, 8
    movgr2fcsr $r3, $a0
  end;

procedure setrm(val: dword);
  begin
    DefaultFPUControlWord.rndmode:=val;
    __setrm(val);
  end;


function getenables: dword; nostackframe; assembler;
  asm
    movfcsr2gr $a0, $r1
  end;


procedure __setenables(enables : dword); nostackframe; assembler;
    asm
      movgr2fcsr $r1, $a0
    end;


procedure setenables(enables : dword);
  begin
    DefaultFPUControlWord.cw:=enables;
    __setenables(enables);
  end;


function getcause: dword; nostackframe; assembler;
  asm
    movfcsr2gr $a0, $r2
    srli.w $a0, $a0, 24
  end;


procedure setcause(cause : dword); nostackframe; assembler;
  asm
    slli.w $a0, $a0, 24
    movgr2fcsr $r2, $a0
  end;


function GetNativeFPUControlWord: TNativeFPUControlWord;
  begin
    result.cw:=getenables;
    result.rndmode:=getrm;
  end;


procedure SetNativeFPUControlWord(const cw: TNativeFPUControlWord);
  begin
    setenables(cw.cw);
    setrm(cw.rndmode);
  end;


procedure RaisePendingExceptions;
  var
    cause : dword;
    f: TFPUException;
  begin
    cause:=getcause;
    if (cause and fpu_i) <> 0 then
      float_raise(exPrecision);
    if (cause and fpu_u) <> 0 then
      float_raise(exUnderflow);
    if (cause and fpu_o) <> 0 then
      float_raise(exOverflow);
    if (cause and fpu_z) <> 0 then
      float_raise(exZeroDivide);
    if (cause and fpu_v) <> 0 then
      float_raise(exInvalidOp);
    { now the soft float exceptions }
    for f in softfloat_exception_flags do
      float_raise(f);
  end;


procedure fpc_throwfpuexception;[public,alias:'FPC_THROWFPUEXCEPTION'];
  var
    cause : dword;
  begin
    cause:=getcause;
    { check, if the exception is masked }
    if ((cause and fpu_i) <> 0) and (exPrecision in softfloat_exception_mask) then
      cause:=cause and not(fpu_i);
    if ((cause and fpu_u) <> 0) and (exUnderflow in softfloat_exception_mask) then
      cause:=cause and not(fpu_u);
    if ((cause and fpu_o) <> 0) and (exOverflow in softfloat_exception_mask) then
      cause:=cause and not(fpu_o);
    if ((cause and fpu_z) <> 0) and (exZeroDivide in softfloat_exception_mask) then
      cause:=cause and not(fpu_z);
    if ((cause and fpu_v) <> 0) and (exInvalidOp in softfloat_exception_mask) then
      cause:=cause and not(fpu_v);
    setcause(cause);
    if cause<>0 then
      RaisePendingExceptions;
  end;


{$define FPC_SYSTEM_HAS_SYSINITFPU}
procedure SysInitFPU;
  begin
    setrm(0);
    setcause(0);
    setenables(fpu_z or fpu_v);
    softfloat_exception_mask:=[exPrecision,exUnderflow];
    softfloat_exception_flags:=[];
  end;


{****************************************************************************
                          Math Routines
****************************************************************************}

{$define FPC_SYSTEM_HAS_SWAPENDIAN}
{TODO It may be better to use the inline method}
{signed 16bit}
function SwapEndian(const AValue: SmallInt): SmallInt;assembler; nostackframe;
  asm
    revb.2h $a0, $a0
    ext.w.h $a0, $a0
  end;
{unsigned 16bit}
function SwapEndian(const AValue: Word): Word;assembler; nostackframe;
  asm
    revb.2h $a0, $a0
    bstrpick.d $a0, $a0, 15, 0
  end;

{signed 32bit}
function SwapEndian(const AValue: LongInt): LongInt; assembler; nostackframe;
  asm
    revb.2w $a0, $a0
    addi.w $a0, $a0, 0
  end;

{unsigned 32bit}
function SwapEndian(const AValue: DWord): DWord; assembler; nostackframe;
  asm
    revb.2w $a0, $a0
    bstrpick.d $a0, $a0, 31, 0
  end;

{signed 64bit}
function SwapEndian(const AValue: Int64): Int64; assembler; nostackframe;
  asm
    revb.d $a0, $a0
  end;

{unsigned 64bit}
function SwapEndian(const AValue: QWord): QWord; assembler; nostackframe;
  asm
    revb.d $a0, $a0
  end;
{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    ori $a0, $fp, 0
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    ld.d $a0, $a0, -8
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;assembler;
  asm
    ld.d $a0, $a0, -16
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    ori $a0, $sp, 0
  end;


function InterLockedDecrement (var Target: longint) : longint; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    addi.w $a1, $zero, -1
    amadd_db.w $a2, $a1, $a0
    add.w $a0, $a1, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.w $a1, $a0, 0
    addi.w $a2, $a1, -1
    sc.w $a2, $a0, 0
    beqz $a2, .LLoop
    addi.w $a0, $a1, -1
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;


function InterLockedIncrement (var Target: longint) : longint; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    addi.w $a1, $zero, 1
    amadd_db.w $a2, $a1, $a0
    add.w $a0, $a1, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.w $a1, $a0, 0
    addi.w $a2, $a1, 1
    sc.w $a2, $a0, 0
    beqz $a2, .LLoop
    addi.w $a0, $a1, 1
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;

function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
    dbar 0
  .LLoop:
    ll.w $a3, $a0, 0
    ori $a2, $a1, 0
    sc.w $a2, $a0, 0
    beqz $a2, .LLoop
    ori $a0, $a3, 0
    dbar 0
  end;


function InterLockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint; assembler; nostackframe;
  asm
    dbar 0
  .LLoop:
    ll.w $a3, $a0, 0
    bne $a3, $a2, .LFail
    ori $a4, $a1, 0
    sc.w $a4, $a0, 0
    beqz $a4, .LLoop
  .LFail:
    ori $a0, $a3, 0
    dbar 0
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    amadd_db.w $a2, $a1, $a0
    move $a0, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.w $a2, $a0, 0
    add.w $a3, $a1, $a2
    sc.w $a3, $a0, 0
    beqz $a3, .LLoop
    move $a0, $a2
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;


function InterLockedDecrement64 (var Target: int64) : int64; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    addi.d $a1, $zero, -1
    amadd_db.d $a2, $a1, $a0
    add.d $a0, $a1, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.d $a1, $a0, 0
    addi.d $a2, $a1, -1
    sc.d $a2, $a0, 0
    beqz $a2, .LLoop
    addi.d $a0, $a1, -1
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;


function InterLockedIncrement64 (var Target: int64) : int64; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    addi.d $a1, $zero, 1
    amadd_db.d $a2, $a1, $a0
    add.d $a0, $a1, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.d $a1, $a0, 0
    addi.d $a2, $a1, 1
    sc.d $a2, $a0, 0
    beqz $a2, .LLoop
    addi.d $a0, $a1, 1
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;


function InterLockedExchange64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
  asm
    dbar 0
  .LLoop:
    ll.d $a3, $a0, 0
    ori $a2, $a1, 0
    sc.d $a2, $a0, 0
    beqz $a2, .LLoop
    ori $a0, $a3, 0
    dbar 0
  end;


function InterLockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64; assembler; nostackframe;
  asm
    dbar 0
  .LLoop:
    ll.d $a3, $a0, 0
    bne $a3, $a2, .LFail
    ori $a4, $a1, 0
    sc.d $a4, $a0, 0
    beqz $a4, .LLoop
  .LFail:
    ori $a0, $a3, 0
    dbar 0
  end;


function InterLockedExchangeAdd64 (var Target: int64;Source : int64) : int64; assembler; nostackframe;
  asm
{$ifdef CPULOONGARCH_HAS_ATOMIC}
    amadd_db.d $a2, $a1, $a0
    add.d $a0, $a1, $a2
{$else CPULOONGARCH_HAS_ATOMIC}
    dbar 0
  .LLoop:
    ll.d $a2, $a0, 0
    add.d $a3, $a1, $a2
    sc.d $a3, $a0, 0
    beqz $a3, .LLoop
    add.d $a0, $a1, $a2
    dbar 0
{$endif CPULOONGARCH_HAS_ATOMIC}
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
  function declocked(var l: longint) : boolean; inline;
  begin
    Result:=InterLockedDecrement(l) = 0;
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
  procedure inclocked(var l: longint); inline;
  begin
    InterLockedIncrement(l);
  end;


{$define FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l:int64):boolean;
  begin
    Result:=InterLockedDecrement64(l) = 0;
  end;


{$define FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l:int64);
  begin
    InterLockedIncrement64(l);
  end;


{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier; assembler; nostackframe;
  asm
    dbar 0
  end;


procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;

procedure ReadWriteBarrier; assembler; nostackframe;
  asm
    dbar 0
  end;

procedure WriteBarrier; assembler; nostackframe;
  asm
    dbar 0
  end;

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE']; assembler; nostackframe;
  asm
    blez $a2, .Lret
    beq  $a0, $a1, .Lret

    move $t0, $a0
    add.d $t1, $a1, $a2
    bgt $t0, $t1, .Lprefast_tail
    sub.d $t1, $a1, $a2
    blt $t0, $t1, .Lprefast_tail
    bgt $a0, $a1, .Lgeneric_head

    add.d $a0, $a0, $a2
    add.d $a1, $a1, $a2
  .Lgeneric_tail:
    ld.b $t0, $a0, -1
    st.b $t0, $a1, -1
    addi.d $a0, $a0, -1
    addi.d $a1, $a1, -1
    addi.d $a2, $a2, -1
    bgtz $a2, .Lgeneric_tail
    b .Lret

  .Lgeneric_head:
    ld.b $t0, $a0, 0
    st.b $t0, $a1, 0
    addi.d $a0, $a0, 1
    addi.d $a1, $a1, 1
    addi.d $a2, $a2, -1
    bgtz $a2, .Lgeneric_head
    b .Lret

  .Lprefast_tail:
    add.d $a0, $a0, $a2
    add.d $a1, $a1, $a2
    ori $a3, $zero, 64
    blt $a2, $a3, .Lgeneric_tail
  .Lfast_tail:
    ld.d $t0, $a0, -8
    ld.d $t1, $a0, -16
    ld.d $t2, $a0, -24
    ld.d $t3, $a0, -32
    ld.d $t4, $a0, -40
    ld.d $t5, $a0, -48
    ld.d $t6, $a0, -56
    ld.d $t7, $a0, -64
    st.d $t0, $a1, -8
    st.d $t1, $a1, -16
    st.d $t2, $a1, -24
    st.d $t3, $a1, -32
    st.d $t4, $a1, -40
    st.d $t5, $a1, -48
    st.d $t6, $a1, -56
    st.d $t7, $a1, -64
    addi.d $a0, $a0, -64
    addi.d $a1, $a1, -64
    addi.d $a2, $a2, -64
    bge $a2, $a3, .Lfast_tail
    bnez $a2, .Lgeneric_tail

  .Lret:
    jr $ra
  end;
{$endif ndef FPC_SYSTEM_HAS_MOVE}

{$define FPC_SYSTEM_HAS_SYSRESETFPU}
procedure SysResetFPU;{$ifdef SYSTEMINLINE}inline;{$endif}
{$ifdef FPUFD}
var
  cw: TNativeFPUControlWord;
{$endif}
begin
  softfloat_exception_flags:=[];
  softfloat_exception_mask:=[exPrecision,exUnderflow];
{$ifdef FPUFD}
  cw:=GetNativeFPUControlWord;
  cw.cw:=0;
  { round to nearest }
  cw.rndmode:=0;
  SetNativeFPUControlWord(cw);
{$endif}
end;
