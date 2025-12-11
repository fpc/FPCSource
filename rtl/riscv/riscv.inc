{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    RiscV which is common to all RiscV types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;
  asm
    addi a0, fp, 0
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    addi a0, sp, 0
  end;

{****************************************************************************
                       fpu exception related stuff
****************************************************************************}

{$ifdef FPUFD}
const
  fpu_nx = 1 shl 0;
  fpu_uf = 1 shl 1;
  fpu_of = 1 shl 2;
  fpu_dz = 1 shl 3;
  fpu_nv = 1 shl 4;

function getfflags: sizeuint; nostackframe; assembler;
  asm
    frflags a0
  end;


procedure setfflags(flags : sizeuint);
  begin
    DefaultFPUControlWord.cw:=flags;
    asm
{$ifdef cpuriscv32}
      lw a0, flags
{$else}
      ld a0, flags
{$endif}
      fsflags a0
    end;
  end;

function getrm: dword; nostackframe; assembler;
  asm
    frrm a0
  end;


procedure setrm(val: dword);
begin
  DefaultFPUControlWord.cw:=val;
  asm
    lw a0, val
    fsrm a0
  end;
end;


function GetNativeFPUControlWord: TNativeFPUControlWord; {$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
    result.cw:=getfflags;
    result.rndmode:=getrm;
  end;


procedure SetNativeFPUControlWord(const cw: TNativeFPUControlWord); {$if defined(SYSTEMINLINE)}inline;{$endif}
  begin
    setfflags(cw.cw);
    setrm(cw.rndmode);
  end;


procedure RaisePendingExceptions;
  var
    fflags : sizeuint;
    f: TFPUException;
  begin
    fflags:=getfflags;
    if (fflags and fpu_dz) <> 0 then
      float_raise(exZeroDivide);
    if (fflags and fpu_of) <> 0 then
      float_raise(exOverflow);
    if (fflags and fpu_uf) <> 0 then
      float_raise(exUnderflow);
    if (fflags and fpu_nv) <> 0 then
      float_raise(exInvalidOp);
    if (fflags and fpu_nx) <> 0 then
      float_raise(exPrecision);
    { now the soft float exceptions }
    for f in softfloat_exception_flags do
      float_raise(f);
  end;


procedure fpc_throwfpuexception;[public,alias:'FPC_THROWFPUEXCEPTION'];
  var
    fflags : sizeuint;
  begin
    fflags:=getfflags;
    { check, if the exception is masked }
    if ((fflags and fpu_dz) <> 0) and (exZeroDivide in softfloat_exception_mask) then
      fflags:=fflags and not(fpu_dz);
    if ((fflags and fpu_of) <> 0) and (exOverflow in softfloat_exception_mask) then
      fflags:=fflags and not(fpu_of);
    if ((fflags and fpu_uf) <> 0) and (exUnderflow in softfloat_exception_mask) then
      fflags:=fflags and not(fpu_uf);
    if ((fflags and fpu_nv) <> 0) and (exInvalidOp in softfloat_exception_mask) then
      fflags:=fflags and not(fpu_nv);
    if ((fflags and fpu_nx) <> 0) and (exPrecision in softfloat_exception_mask) then
      fflags:=fflags and not(fpu_nx);
    setfflags(fflags);
    if fflags<>0 then
      RaisePendingExceptions;
  end;
{$endif FPUFD}


{$define FPC_SYSTEM_HAS_SYSINITFPU}
procedure SysInitFPU;
{$ifdef FPUFD}
var
  cw: TNativeFPUControlWord;
{$endif}
begin
  softfloat_exception_flags:=[];
  softfloat_exception_mask:=[exPrecision,exUnderflow];
{$ifdef FPUFD}
  cw:=GetNativeFPUControlWord;
  { riscv does not support triggering exceptions when FPU exceptions happen;
    it merely records which exceptions have happened until now -> clear }
  cw.cw:=0;
  { round to nearest }
  cw.rndmode:=0;
  SetNativeFPUControlWord(cw);
{$endif}
end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
{$ifdef FPUFD}
var
  cw: TNativeFPUControlWord;
{$endif}
begin
  softfloat_exception_flags:=[];
{$ifdef FPUFD}
  { clear all "exception happened" flags we care about}
  cw:=GetNativeFPUControlWord;
  cw.cw:=0;
  SetNativeFPUControlWord(cw);
{$endif FPUFD}
end;


{$define FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier; assembler; nostackframe;
  asm
    fence ir, ir
  end;


procedure ReadDependencyBarrier;
  begin
  end;


procedure ReadWriteBarrier; assembler; nostackframe;
  asm
    fence iorw, iorw
  end;


procedure WriteBarrier; assembler; nostackframe;
  asm
    fence ow, ow
  end;

{****************************************************************************
                       atomic operations
****************************************************************************}

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


{$ifdef CPURV_HAS_ZBB}

{$define FPC_SYSTEM_HAS_SWAPENDIAN}

function SwapEndian(const AValue: SmallInt): SmallInt; assembler; nostackframe;
asm
  rev8 a0, a0
  srli a0, a0, 16
end;


function SwapEndian(const AValue: Word): Word; assembler; nostackframe;
asm
  rev8 a0, a0
  srli a0, a0, 16
end;


function SwapEndian(const AValue: LongInt): LongInt; assembler; nostackframe;
asm
  rev8 a0, a0
end;


function SwapEndian(const AValue: DWord): DWord; assembler; nostackframe;
asm
  rev8 a0, a0
end;


function SwapEndian(const AValue: Int64): Int64; assembler; nostackframe;
asm
  rev8 a2, a0
  rev8 a0, a1
  addi a1, a2, 0
end;


function SwapEndian(const AValue: QWord): QWord; assembler; nostackframe;
asm
  rev8 a2, a0
  rev8 a0, a1
  addi a1, a2, 0
end;

{$endif CPURV_HAS_ZBB}
