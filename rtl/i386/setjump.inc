{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
           Set_Jmp/Long_jmp
 **********************************************************************}

{$I386_DIRECT}
Function SetJmp (Var S : Jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];

asm
  movl 8(%ebp),%eax
  movl %ebx,(%eax)
  movl %esi,4(%eax)
  movl %edi,8(%eax)
  movl 4(%ebp),%edx
  movl %edx,20(%eax)
  movl (%ebp),%edx
  movl %edx,12(%eax)
  leal 8(%ebp),%edx
  movl %edx,16(%eax)
  xorl %eax,%eax
end;

Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];

asm
  movl 8(%ebp),%ecx
  movl 12(%ebp),%eax
  testl %eax,%eax
  jne .nonzero
  movl $1,%eax
.nonzero:
  movl (%ecx),%ebx
  movl 4(%ecx),%esi
  movl 8(%ecx),%edi
  movl 12(%ecx),%ebp
  movl 16(%ecx),%esp
  jmp *20(%ecx)
end;

{ I386_ATT removed for bugfix branch }

