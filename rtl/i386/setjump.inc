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
  movl 4(%ebp),%edx
  movl %edx,20(%eax)
  movl (%ebp),%edx
  movl %edx,12(%eax)
  leal 12(%ebp),%edx
  movl %edx,16(%eax)
  xorl %eax,%eax
end;


Procedure longJmp (Var S : Jmp_buf; value : longint); assembler;[Public, alias : 'FPC_LONGJMP'];
asm
  movl 8(%ebp),%ecx
  movl 12(%ebp),%eax
  testl %eax,%eax
  jne .Ljnonzero
  movl $1,%eax
.Ljnonzero:
  movl (%ecx),%ebx
  movl 4(%ecx),%esi
  movl 8(%ecx),%edi
  movl 12(%ecx),%ebp
  movl 16(%ecx),%esp
  // we should also clear the fpu
  fninit
  jmp 20(%ecx)
end;

{
  $Log$
  Revision 1.11  2000-02-18 16:16:13  florian
    * we don't need to to finit twice ...

  Revision 1.10  2000/02/18 15:23:01  florian
    * fixed constants in rtti
    * longjmp does now a finit

  Revision 1.9  2000/02/09 22:12:54  florian
    * longjump does now a finit

  Revision 1.8  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.7  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.6  1999/08/18 10:42:13  pierre
   * loading of esp value corrected
}
