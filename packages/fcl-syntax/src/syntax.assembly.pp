{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Assembly language syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.assembly;

interface

uses
  syntax.highlighter, syntax.table;

type

  { TAssemblySyntaxHighlighter }

  TAssemblySyntaxHighlighter = class(TTableSyntaxHighlighter)
  protected
    class function Describe: TLanguageDescription; override;
  end;

implementation

const
  AssemblyWords =
    'adc add adds adcs adr adrp and ands asr b bl blr br bne beq bgt bge blt ble bic call cbnz cbz clc cld cli cmn cmp csel dec div eor fadd fdiv fmul fsub hlt idiv imul inc int ja jae jb jbe je jg jge jl jle jmp jne jnz jz ldp ldr ldrb ldrh lea leave lock lsl lsr mov movabs movk movn movs movsx movsxd movw movz movzx mul neg nop not or orr pop push rep ret rol ror sal sar sbb scasb setz shl shr stp str strb strh sub subs svc syscall sysenter tbz tbnz test ud2 udiv xor';
  AssemblyRegisters =
    'al ah ax eax rax bl bh bx ebx rbx cl ch cx ecx rcx dl dh dx edx rdx si esi rsi di edi rdi sp esp rsp bp ebp rbp ip eip rip r8 r9 r10 r11 r12 r13 r14 r15 r8d r9d r10d r11d r12d r13d r14d r15d cs ds es fs gs ss xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 ymm0 ymm1 ymm2 ymm3 zmm0 zmm1 zmm2 zmm3 r0 r1 r2 r3 r4 r5 r6 r7 lr pc x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 xzr w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 wzr';

class function TAssemblySyntaxHighlighter.Describe: TLanguageDescription;

begin
  Result := Language('asm nasm gas x86 arm', AssemblyWords, AssemblyRegisters + ' byte word dword qword oword tword ptr rel abs near far short');
  Result.Flavor := lfAssembly;
  Result.CaseSensitive := False;
  LineComment(Result, ';');
  LineComment(Result, '//');
  StringForm(Result, '"', '"');
  StringForm(Result, '''', '''');
  Result.IdentifierStart := Result.IdentifierStart + ['.', '$'];
  Result.IdentifierRest := Result.IdentifierRest + ['.', '$'];
  Result.Directive := '%';
  Result.Numbers.Hex := True;
  Result.Numbers.Binary := True;
  Result.Numbers.Octal := True;
  Result.Numbers.TrailingBase := True;
end;

initialization
  TAssemblySyntaxHighlighter.Register;
end.
