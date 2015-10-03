{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, not shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifndef FPC_UNIT_HAS_STRPCOPY}
{$define FPC_UNIT_HAS_STRPCOPY}
function strpcopy(d : pchar;const s : string) : pchar;assembler;nostackframe;
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
  mov dx, ds  // for far data models, backup ds; for near data models, use to initialize es
{$ifdef FPC_X86_DATA_NEAR}
  mov es, dx
  mov si, ss:[bx + 2 + extra_param_offset]  // @s
  mov di, ss:[bx + 4 + extra_param_offset]  // @d
{$else FPC_X86_DATA_NEAR}
  lds si, ss:[bx + 2 + extra_param_offset]  // @s
  les di, ss:[bx + 6 + extra_param_offset]  // @d
{$endif FPC_X86_DATA_NEAR}
  // we will no longer use bx for reading parameters, so save di there
  // in order to be able to return it in the end
  mov bx, di

{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  lodsb        // load length in al
  xor ah, ah
  xchg cx, ax  // 1 byte shorter than mov
  shr cx, 1
  rep movsw
  adc cx, cx
  rep movsb
  xchg ax, cx  // ax := 0 (1 byte shorter than xor al, al)
  stosb        // zero terminate the destination string
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  mov ds, dx
  mov dx, es   // return segment of d in dx
{$endif}
  xchg ax, bx  // return original offset of d in ax
end;
{$endif FPC_UNIT_HAS_STRPCOPY}
