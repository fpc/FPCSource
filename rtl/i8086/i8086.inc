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


{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];assembler;nostackframe;
asm
  mov bx, sp
  mov cx, ss:[bx + 2 + extra_param_offset]  // count
  or cx, cx
  jle @@Done

  mov ax, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, ax
  mov si, ss:[bx + 6 + extra_param_offset]  // @source
  mov di, ss:[bx + 4 + extra_param_offset]  // @dest
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 8 + extra_param_offset]  // @source
  les di, ss:[bx + 4 + extra_param_offset]  // @dest
{$endif FPC_X86_DATA_NEAR}

  cmp si, di
  jb @@BackwardsMove

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  shr cx, 1
  rep movsw
  adc cx, cx
  rep movsb
  jmp @@AfterMove  // todo, add mov ds,ax & ret here for performance reasons

@@BackwardsMove:
  std
  add si, cx
  add di, cx
  dec si
  dec di
  rep movsb  // todo: movsw
  cld

@@AfterMove:
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, ax
{$endif}

@@Done:
end;
{$endif FPC_SYSTEM_HAS_MOVE}


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;nostackframe;
asm
  mov ax, sp
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov dx, ss
{$endif}
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
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov dx, ss
{$endif}
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
  mov si, sp
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
  pushf
  cli
  sub word [bx], 1
  sbb word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  popf
end;

{TODO: fix, use smallint?}
function InterLockedIncrement (var Target: longint) : longint;nostackframe;assembler;
asm
  mov si, sp
  mov bx, ss:[si + 2 + extra_param_offset]  // Target
  pushf
  cli
  add word [bx], 1
  adc word [bx+2], 0
  mov ax, [bx]
  mov dx, [bx+2]
  popf
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
                                  BSR/BSF
****************************************************************************}

const
  bsr8bit: array [Byte] of Byte = (
    $ff,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
      5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    );
  bsf8bit: array [Byte] of Byte = (
    $ff,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      7,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,
      5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
    );

{$define FPC_SYSTEM_HAS_BSR_BYTE}
function BsrByte(const AValue: Byte): Byte;
begin
  BsrByte := bsr8bit[AValue];
end;

{$define FPC_SYSTEM_HAS_BSF_BYTE}
function BsfByte(const AValue: Byte): Byte;
begin
  BsfByte := bsf8bit[AValue];
end;

{$define FPC_SYSTEM_HAS_BSR_WORD}
function BsrWord(const AValue: Word): Byte; assembler;
asm
     lea   bx, bsr8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ah, ah
     jz    @@0
     mov   cl, 8
     mov   al, ah
@@0: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_WORD}
function BsfWord(const AValue: Word): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  al, al
     jnz   @@0
     or    al, ah
     jz    @@0
     add   cl, 8
@@0: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSR_DWORD}
function BsrDword(const AValue: DWord): Byte; assembler;
asm
     lea   bx, bsr8bit
     mov   cl, 16
     mov   ax, word [AValue+2]
     test  ax, ax
     jnz   @@0
     xor   cl, cl
     mov   ax, word [AValue]
@@0: test  ah, ah
     jz    @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_DWORD}
function BsfDword(const AValue: DWord): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ax, ax
     jnz   @@0
     or    ax, word [AValue+2]
     jz    @@1
     mov   cl, 16
@@0: test  al, al
     jnz   @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSR_QWORD}
function BsrQword(const AValue: QWord): Byte; assembler;
asm
     lea   bx, bsr8bit
     mov   cl, 48
     mov   ax, word [AValue+6]
     test  ax, ax
     jnz   @@0
     mov   cl, 32
     or    ax, word [AValue+4]
     jnz   @@0
     mov   cl, 16
     or    ax, word [AValue+2]
     jnz   @@0
     xor   cl, cl
     mov   ax, word [AValue]
@@0: test  ah, ah
     jz    @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;

{$define FPC_SYSTEM_HAS_BSF_QWORD}
function BsfQword(const AValue: QWord): Byte; assembler;
asm
     lea   bx, bsf8bit
     xor   cl, cl
     mov   ax, word [AValue]
     test  ax, ax
     jnz   @@0
     mov   cl, 16
     or    ax, word [AValue+2]
     jnz   @@0
     mov   cl, 32
     or    ax, word [AValue+4]
     jnz   @@0
     xor   cl, cl
     or    ax, word [AValue+6]
     jz    @@1
     mov   cl, 48
@@0: test  al, al
     jnz   @@1
     add   cl, 8
     mov   al, ah
@@1: xlatb
     add   al, cl
end;


{****************************************************************************
                                  HexStr
****************************************************************************}

{$define FPC_HAS_HEXSTR_POINTER_SHORTSTR}
function HexStr(Val: NearPointer): ShortString;
begin
  HexStr:=HexStr(Word(Val),4);
end;

function HexStr(Val: FarPointer): ShortString;
type
  TFarPointerRec = record
    Offset, Segment: Word;
  end;
begin
  HexStr:=HexStr(TFarPointerRec(Val).Segment,4)+':'+HexStr(TFarPointerRec(Val).Offset,4);
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

{ Detects the FPU and initializes the Test8087 variable (and Default8087CW):
  0 = NO FPU
  1 = 8087
  2 = 80287
  3 = 80387+ }
procedure DetectFPU;
  var
    localfpucw: word;
  begin
    asm
      xor bx, bx  { initialization, 0=NO FPU }

      { FPU presence detection }
      fninit
      mov byte [localfpucw + 1], 0
      nop
      fnstcw localfpucw
      cmp byte [localfpucw + 1], 3
      jne @@Done  { No FPU? }
      inc bx

      { FPU found; now test if it's a 8087 }
      and byte [localfpucw], $7F  { clear the interrupt enable mask (IEM) }
      fldcw localfpucw
      fdisi                       { try to set the interrupt enable mask }
      fstcw localfpucw
      test byte [localfpucw], $80 { IEM set? }
      jnz @@Done  { if yes, we have an 8087 }
      inc bx

      { we have a 287+; now test if it's a 80287 }
      finit
      fld1
      fldz
      fdiv                        { calculate 1/0 }
      fld st                      { copy the value }
      fchs                        { change the sign }
      fcompp  { compare. if the FPU distinguishes +inf from -inf, it's a 387+ }
      fstsw localfpucw
      mov ah, byte [localfpucw + 1]
      sahf
      je @@Done
      inc bx  { 387+ }
@@Done:
      mov Test8087, bl
    end ['AX','BX'];
    if Test8087<=2 then
      Default8087CW:=$1330
    else
      Default8087CW:=$1332;
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
  end;

{$I int32p.inc}

