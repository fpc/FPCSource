{
    $Id$
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
  movl 8(%ebp),%eax
  movl %ebx,(%eax)
  movl %esi,4(%eax)
  movl %edi,8(%eax)
  movl 4(%ebp),%edi
  movl %edi,20(%eax)
  movl (%ebp),%edi
  movl %edi,12(%eax)
  leal 12(%ebp),%edi
  movl %edi,16(%eax)
  movl 8(%eax),%edi
  xorl %eax,%eax
end['EAX'];


Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
  movl 8(%ebp),%ecx
  movl 12(%ebp),%eax
  movl (%ecx),%ebx
  movl 4(%ecx),%esi
  movl 8(%ecx),%edi
  movl 12(%ecx),%ebp
  movl 16(%ecx),%esp
  // we should also clear the fpu
  // fninit no must be done elsewhere PM
  // or we should reset the control word also
  jmp 20(%ecx)
end;

{
  $Log$
  Revision 1.3  2002-09-07 16:01:19  peter
    * old logs removed and tabs fixed

}
