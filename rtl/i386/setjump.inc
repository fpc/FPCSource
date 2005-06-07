{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function SetJmp (Var S : Jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
asm
{$ifndef REGCALL}
  movl 8(%ebp),%eax
{$endif}
  movl %ebx,(%eax)
  movl %esi,4(%eax)
  movl %edi,8(%eax)
  movl 4(%ebp),%edi
  movl %edi,20(%eax)
  movl (%ebp),%edi
  movl %edi,12(%eax)
{$ifdef REGCALL}
  leal 8(%ebp),%edi
{$else}
  leal 12(%ebp),%edi
{$endif}
  movl %edi,16(%eax)
  movl 8(%eax),%edi
  xorl %eax,%eax
end['EAX'];


Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
{$ifdef REGCALL}
  xchgl %edx,%eax
{$else}
  movl 8(%ebp),%edx
  movl 12(%ebp),%eax
{$endif}

  movl (%edx),%ebx
  movl 4(%edx),%esi
  movl 8(%edx),%edi
  movl 12(%edx),%ebp
  movl 16(%edx),%esp
  // we should also clear the fpu
  // fninit no must be done elsewhere PM
  // or we should reset the control word also
  jmp 20(%edx)
end;

