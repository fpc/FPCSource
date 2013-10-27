{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i8086+

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_cpuinit;
  begin
  end;

{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure FillChar(var x;count:SizeInt;value:byte);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // count
  or cx, cx
  jle @@Done
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @x
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @x
{$endif FPC_X86_DATA_NEAR}
  mov al, ss:[bx + 2 + extra_param_offset]  // value
  mov ah, al
  shr cx, 1
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  rep stosw
  adc cx, cx
  rep stosb
@@Done:
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
{$define FPC_SYSTEM_HAS_FILLWORD}
procedure FillWord(var x;count : SizeInt;value : word);assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 4 + extra_param_offset]  // count
  or cx, cx
  jle @@Done
{$ifdef FPC_X86_DATA_NEAR}
  mov di, ss:[bx + 6 + extra_param_offset]  // @x
  mov ax, ds
  mov es, ax
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 6 + extra_param_offset]  // @x
{$endif FPC_X86_DATA_NEAR}
  mov ax, ss:[bx + 2 + extra_param_offset]  // value
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  rep stosw
  @@Done:
end;
{$endif FPC_SYSTEM_HAS_FILLWORD}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;
asm
  mov ax, sp
end;

{$define FPC_SYSTEM_HAS_PTR}
function Ptr(sel,off: Word):farpointer;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov si, sp
  mov ax, ss:[si + 2 + extra_param_offset]  // off
  mov dx, ss:[si + 4 + extra_param_offset]  // sel
end;

{$define FPC_SYSTEM_HAS_CSEG}
function CSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, cs
end;

{$define FPC_SYSTEM_HAS_DSEG}
function DSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, ds
end;

{$define FPC_SYSTEM_HAS_SSEG}
function SSeg: Word;{$ifdef SYSTEMINLINE}inline;{$endif}assembler;nostackframe;
asm
  mov ax, ss
end;

{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  mov ax, bp
end;
{$ENDIF not INTERNAL_BACKTRACE}

{$define FPC_SYSTEM_HAS_GET_PC_ADDR}
Function Get_pc_addr : CodePointer;assembler;nostackframe;
asm
  mov bx, sp
  mov ax, ss:[bx]
{$ifdef FPC_X86_CODE_FAR}
  mov dx, ss:[bx+2]
{$endif FPC_X86_CODE_FAR}
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer;addr:codepointer=nil):codepointer;nostackframe;assembler;
asm
  mov si, sp
{$ifdef FPC_X86_CODE_FAR}
  xor dx, dx
{$endif FPC_X86_CODE_FAR}
  mov ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
  or ax, ax
  jz @@Lg_a_null
  xchg ax, bx  // 1 byte shorter than a mov
  mov ax, [bx+2]
{$ifdef FPC_X86_CODE_FAR}
  mov dx, [bx+4]
{$endif FPC_X86_CODE_FAR}
@@Lg_a_null:
end;

{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer;addr:codepointer=nil):pointer;nostackframe;assembler;
asm
  mov si, sp
  mov ax, ss:[si + 4 + extra_param_offset + extra_param_offset]  // framebp
  or ax, ax
  jz @@Lgnf_null
  xchg ax, si  // 1 byte shorter than a mov
  lodsw
@@Lgnf_null:
end;

{TODO: fix, use smallint?}
function InterLockedDecrement (var Target: longint) : longint;nostackframe;assembler;
asm
  push bp
  mov bp, sp
  mov bx, ss:[bp + 4 + extra_param_offset]  // Target
  sub word [bx], 1
  sbb word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  pop bp
end;

{TODO: fix, use smallint?}
function InterLockedIncrement (var Target: longint) : longint;nostackframe;assembler;
asm
  push bp
  mov bp, sp
  mov bx, ss:[bp + 4 + extra_param_offset]  // Target
  add word [bx], 1
  adc word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  pop bp
end;

{TODO: fix, use smallint?}
function InterLockedExchange (var Target: longint;Source : longint) : longint;
begin
  InterLockedExchange := Target;
  Target := Source;
end;

{TODO: implement}
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
begin
  runerror(304);
end;

{TODO: implement}
function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
begin
  runerror(304);
end;

{****************************************************************************
                                  FPU
****************************************************************************}

const
  { Internal constants for use in system unit }
  FPU_Invalid = 1;
  FPU_Denormal = 2;
  FPU_DivisionByZero = 4;
  FPU_Overflow = 8;
  FPU_Underflow = $10;
  FPU_StackUnderflow = $20;
  FPU_StackOverflow = $40;
  FPU_ExceptionMask = $ff;

  { use Default8087CW instead
  fpucw : word = $1300 or FPU_StackUnderflow or FPU_Underflow or FPU_Denormal;
  }

{ returns true if FPU is present }
function DetectFPU: boolean;
  var
    localfpucw: word;
  begin
    asm
      fninit
      mov byte [localfpucw + 1], 0
      fnstcw localfpucw
    end;
    DetectFPU:=(localfpucw and $FF00)=$0300;
  end;

{$define FPC_SYSTEM_HAS_SYSINITFPU}
Procedure SysInitFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    asm
      fninit
      fldcw   localfpucw
      fwait
    end;
    softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
  end;


{$define FPC_SYSTEM_HAS_SYSRESETFPU}
Procedure SysResetFPU;
  var
    { these locals are so we don't have to hack pic code in the assembler }
    localmxcsr: dword;
    localfpucw: word;
  begin
    localfpucw:=Default8087CW;
    asm
      fninit
      fwait
      fldcw   localfpucw
    end;
    softfloat_exception_flags:=0;
  end;

