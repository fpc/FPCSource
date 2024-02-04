{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ASMMODE GAS}

{$ifdef FPC_HAS_TYPE_EXTENDED}
{$define FPC_MATH_HAS_ARCTAN2}
function arctan2(y,x : float) : float;assembler;nostackframe;
  asm
     fldt 8(%rsp)
     fldt 24(%rsp)
     fpatan
     fwait
  end;

{$define FPC_MATH_HAS_TAN}
function tan(x : float) : float;assembler;nostackframe;
  asm
    fldt 8(%rsp)
    fptan
    fstp %st
    fwait
  end;


{$define FPC_MATH_HAS_COTAN}
function cotan(x : float) : float;assembler;nostackframe;
  asm
    fldt 8(%rsp)
    fptan
    fdivp %st,%st(1)
    fwait
  end;


{$define FPC_MATH_HAS_LOG2}
function log2(x : float) : float;assembler;nostackframe;
  asm
    fld1
    fldt 8(%rsp)
    fyl2x
    fwait
  end;
{$endif FPC_HAS_TYPE_EXTENDED}


{$define FPC_MATH_HAS_SINCOS}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure sincos(theta : extended;out sinus,cosinus : extended);assembler;
asm
    fldt theta
    fsincos
{$ifdef WIN64}
    fstpl (%r8)
    fstpl (%rdx)
{$else WIN64}
    fstpt (%rsi)
    fstpt (%rdi)
{$endif WIN64}
    fwait
  end;
{$endif FPC_HAS_TYPE_EXTENDED}


{$asmmode intel}
procedure sincos(theta : double;out sinus,cosinus : double);assembler;
  var
    t : double;
  asm
    movsd qword ptr t,xmm0
    fld qword ptr t
    fsincos
    fstp qword ptr [cosinus]
    fstp qword ptr [sinus]
    fwait
  end;


procedure sincos(theta : single;out sinus,cosinus : single);assembler;
  var
    t : single;
  asm
    movss dword ptr t,xmm0
    fld dword ptr t
    fsincos
    fstp dword ptr [cosinus]
    fstp dword ptr [sinus]
    fwait
  end;


{$define FPC_MATH_HAS_DIVMOD}
{$asmmode intel}
procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);assembler;
asm
{$ifdef WIN64}
  mov eax, ecx
  movzx ecx, dx
  cdq
  idiv ecx
  mov [r8], ax
  mov [r9], dx
{$else WIN64}
  mov eax, edi
  movzx esi, si
  mov rdi, rdx
  cdq
  idiv esi
  mov [rdi], ax
  mov [rcx], dx
{$endif WIN64}
end;


procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: SmallInt);assembler;
asm
{$ifdef WIN64}
  mov eax, ecx
  movzx ecx, dx
  cdq
  idiv ecx
  mov [r8], ax
  mov [r9], dx
{$else WIN64}
  mov eax, edi
  movzx esi, si
  mov rdi, rdx
  cdq
  idiv esi
  mov [rdi], ax
  mov [rcx], dx
{$endif WIN64}
end;


procedure DivMod(Dividend: DWord; Divisor: DWord; var Result, Remainder: DWord);assembler;
asm
{$ifdef WIN64}
  mov eax, ecx
  mov ecx, edx
  xor edx, edx
  div ecx
  mov [r8], eax
  mov [r9], edx
{$else WIN64}
  mov eax, edi
  mov rdi, rdx
  xor edx, edx
  div esi
  mov [rdi], eax
  mov [rcx], edx
{$endif WIN64}
end;


procedure DivMod(Dividend: Integer; Divisor: Integer; var Result, Remainder: Integer);assembler;
asm
{$ifdef WIN64}
  mov eax, ecx
  mov ecx, edx
  cdq
  idiv ecx
  mov [r8], eax
  mov [r9], edx
{$else WIN64}
  mov eax, edi
  mov rdi, rdx
  cdq
  idiv esi
  mov [rdi], eax
  mov [rcx], edx
{$endif WIN64}
end;
