{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$if defined(FPC_MM_TINY)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_SMALL)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_MEDIUM)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_COMPACT)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_FAR}
{$elseif defined(FPC_MM_LARGE)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_FAR}
{$elseif defined(FPC_MM_HUGE)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_HUGE}
{$else}
  {$fatal No memory model defined}
{$endif}

{$ifndef FPC_UNIT_HAS_STREND}
{$define FPC_UNIT_HAS_STREND}
function strend(p : pchar) : pchar;assembler;nostackframe;
const
  { used for an offset fixup for accessing the proc parameters in asm routines
    that use nostackframe. We can't use the parameter name directly, because
    i8086 doesn't support sp relative addressing. }
{$ifdef FPC_X86_CODE_FAR}
  extra_param_offset = 2;
{$else FPC_X86_CODE_FAR}
  extra_param_offset = 0;
{$endif FPC_X86_CODE_FAR}
asm
  mov bx, sp
  xor ax, ax
{$ifdef FPC_X86_DATA_NEAR}
  mov dx, ds
  mov es, dx
  mov di, ss:[bx + 2 + extra_param_offset]  // @p
  or di, di
  jz @@LStrEndNil
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 2 + extra_param_offset]  // @p
  mov dx, es
  mov cx, dx
  or cx, di
  jz @@LStrEndNil
{$endif FPC_X86_DATA_NEAR}
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  mov cx, 0ffffh
  repne scasb
  mov ax, di
  dec ax
@@LStrEndNil:
end;
{$endif FPC_UNIT_HAS_STREND}


{$ifndef FPC_UNIT_HAS_STRUPPER}
{$define FPC_UNIT_HAS_STRUPPER}
function strupper(p : pchar) : pchar;assembler;nostackframe;
const
{$ifdef FPC_X86_CODE_FAR}
  extra_param_offset = 2;
{$else FPC_X86_CODE_FAR}
  extra_param_offset = 0;
{$endif FPC_X86_CODE_FAR}
asm
  mov bx, sp
  xor ax, ax
{$ifdef FPC_X86_DATA_NEAR}
  mov dx, ds
  mov es, dx
  mov di, ss:[bx + 2 + extra_param_offset]  // @p
  or di, di
  jz @@LStrUpperNil
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 2 + extra_param_offset]  // @p
  mov dx, es
  mov cx, dx
  or cx, di
  jz @@LStrUpperNil
  mov cx, ds
  mov ds, dx
{$endif FPC_X86_DATA_NEAR}
  mov si, di
  mov bx, di
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
@@LSTRUPPER1:
  lodsb
  cmp al, 'a'
  jb @@LSTRUPPER3
  cmp al, 'z'
  ja @@LSTRUPPER3
  sub al, 20h
@@LSTRUPPER3:
  stosb
  or al, al
  jnz @@LSTRUPPER1
  xchg ax, bx  { 1 byte shorter than a mov }
{$ifndef FPC_X86_DATA_NEAR}
  mov ds, cx
{$endif not FPC_X86_DATA_NEAR}
@@LStrUpperNil:
end;
{$endif FPC_UNIT_HAS_STRUPPER}


{$ifndef FPC_UNIT_HAS_STRLOWER}
{$define FPC_UNIT_HAS_STRLOWER}
function strlower(p : pchar) : pchar;assembler;nostackframe;
const
{$ifdef FPC_X86_CODE_FAR}
  extra_param_offset = 2;
{$else FPC_X86_CODE_FAR}
  extra_param_offset = 0;
{$endif FPC_X86_CODE_FAR}
asm
  mov bx, sp
  xor ax, ax
{$ifdef FPC_X86_DATA_NEAR}
  mov dx, ds
  mov es, dx
  mov di, ss:[bx + 2 + extra_param_offset]  // @p
  or di, di
  jz @@LStrLowerNil
{$else FPC_X86_DATA_NEAR}
  les di, ss:[bx + 2 + extra_param_offset]  // @p
  mov dx, es
  mov cx, dx
  or cx, di
  jz @@LStrLowerNil
  mov cx, ds
  mov ds, dx
{$endif FPC_X86_DATA_NEAR}
  mov si, di
  mov bx, di
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
@@LSTRLOWER1:
  lodsb
  cmp al, 'A'
  jb @@LSTRLOWER3
  cmp al, 'Z'
  ja @@LSTRLOWER3
  add al, 20h
@@LSTRLOWER3:
  stosb
  or al, al
  jnz @@LSTRLOWER1
  xchg ax, bx  { 1 byte shorter than a mov }
{$ifndef FPC_X86_DATA_NEAR}
  mov ds, cx
{$endif not FPC_X86_DATA_NEAR}
@@LStrLowerNil:
end;
{$endif FPC_UNIT_HAS_STRLOWER}
